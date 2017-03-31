%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% --------------------------------------------------
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%% --------------------------------------------------
%% File    : ct_expand2.erl
%% @author  : Ulf Wiger <ulf@wiger.net>
%% @end
%% Created : 7 Apr 2010 by Ulf Wiger <ulf@wiger.net>
%%-------------------------------------------------------------------

%% @doc Compile-time expansion utility
%%
%% This module serves as an example of parse_trans-based transforms,
%% but might also be a useful utility in its own right.
%% The transform searches for calls to the pseudo-function
%% `ct_expand2:term(Expr)', and then replaces the call site with the
%% result of evaluating `Expr' at compile-time.
%%
%% For example, the line
%%
%% `ct_expand2:term(lists:sort([3,5,2,1,4]))'
%%
%% would be expanded at compile-time to `[1,2,3,4,5]'.
%%
%% ct_expand2 has now been extended to also evaluate calls to local functions.
%% See examples/ct_expand_test.erl for some examples.
%%
%% A debugging facility exists: passing the option {ct_expand_trace, Flags} as an option,
%% or adding a compiler attribute -ct_expand_trace(Flags) will enable a form of call trace.
%%
%% `Flags' can be `[]' (no trace) or `[F]', where `F' is `c' (call trace),
%% `r' (return trace), or `x' (exception trace)'.
%%
%% @end
-module(ct_expand2).
-export([parse_transform/2]).

-export([extract_fun/3,
    lfun_rewrite/2]).

-define(DEBUG, true).
-include("func_logger.hrl").

-type form() :: any().
-type forms() :: [form()].
-type options() :: [{atom(), any()}].

-spec parse_transform(forms(), options()) ->
    forms().


parse_transform(Forms0, Options) ->
    Forms = erl_expand_records:module(Forms0, []),
    Trace = ct_trace_opt(Options, Forms),
    case parse_trans:depth_first(
        fun(T, F, C, A) ->
            xform_fun(T, F, C, A, Forms, Trace)
        end, [], Forms, Options) of
        {error, Es} ->
            Es ++ Forms;
        {NewForms, _} ->
            parse_trans:revert(NewForms)
    end.

ct_trace_opt(Options, Forms) ->
    case proplists:get_value(ct_expand_trace, Options) of
        undefined ->
            case [Opt || {attribute, _, ct_expand_trace, Opt} <- Forms] of
                [] ->
                    [];
                [_ | _] = L ->
                    lists:last(L)
            end;
        Flags when is_list(Flags) ->
            Flags
    end.

xform_fun(application, Form, _Ctxt, Acc, Forms, Trace) ->
    MFA = erl_syntax_lib:analyze_application(Form),
    case MFA of
        {?MODULE, {term, _}} ->
            LFH = fun(Name, Args, Bs) ->
                eval_lfun(
                    extract_fun(Name, length(Args), Forms),
                    Args, Bs, Forms, Trace)
                  end,
            Args = erl_syntax:application_arguments(Form),
            RevArgs = parse_trans:revert(Args),
            case erl_eval2:exprs(RevArgs, [], {eval, LFH}, nlf()) of
                {value, {call, _, {'fun', _, _Cs}, _As} = Func, _} ->
                    {_, Expr, Bs} = expand(Forms, Func, erl_eval2:new_bindings()),
                    {Expr, Acc};
                {value, Value, _} ->
                    {abstract(Value), Acc};
                Other ->
                    io:format("cannot evaluate: ~p~n", [Other])
            end;
        _ ->
            {Form, Acc}
    end;
xform_fun(_, Form, _Ctxt, Acc, _, _) ->
    {Form, Acc}.

remote_fun({M, F}, Args) -> apply(M, F, Args).
nlf() -> {value, fun(A,B) -> remote_fun(A, B) end}.

raw_expand(Forms, Expr, Bs) -> erl_eval2:expr(Expr, Bs, lfh(Forms, []), nlf()).

?funclog("1:").
expand(Forms, {call, _, {'fun', _, {clauses, Cs}}, As}, Bs) ->
    expand_lfun(Forms, Cs, As, Bs, none, none, none);
expand(Forms, {'case', _, Cond, Cs} = CaseStatment, Bs) ->
    try
        begin
            {value, Val, Bs2} = raw_expand(Forms, Cond, Bs),
            case erl_eval2:match_clause(Cs, [Val], Bs2, lfh(Forms, [])) of
                {ClauseBody, Bs3} ->
                    {Result, Bs4} = expand_exprs(Forms, ClauseBody, Bs3),
                    {ok, Result, Bs4};
                notmatch ->
                    {error, CaseStatment, Bs}
            end
        end
    catch
        error:Err ->
            io:format("expand, exception, expr:~p, bs:~p, err:~p~n", [CaseStatment, Bs, Err]),
            {error, CaseStatment, Bs}
    end;

expand(Forms, Expr, Bs_) ->
    try
        begin
            {value, AV, Bs1_} = raw_expand(Forms, Expr, Bs_),
            {ok, AV, Bs1_}
        end
    catch
        error:Err ->
            io:format("expand, exception, expr:~p, bs:~p, err:~p~n", [Expr, Bs_, Err]),
            {error, Expr, Bs_}
    end.

?funclog("1:").
expand_exprs(Forms, Exprs, Bs) ->
    {NewExprs, {Bs3, Err}} = lists:mapfoldl(
        fun(Expr, {Bs_, Error}) ->
            case expand(Forms, Expr, Bs_) of
                {ok, Result, Bs2} -> {{ok, Result}, {Bs2, Error}};
                {error, Result, Bs2} -> {{error, Result}, {Bs2, true}}
            end
        end, {Bs, false}, Exprs),
    if
        Err -> {{block, 0, [
            case State of
                ok -> abstract(Result);
                error -> Result
            end || {State, Result} <- NewExprs]}, Bs3};
        true -> {element(2, lists:last(NewExprs)), Bs3}
    end.

?funclog("1:").
expand_lfun(Forms, [{clause, _, H, G, Exprs} | Cs], As0, Bs_in, Lf, Ef, RBs) ->
    {As, Bs0} = lists:mapfoldl(
        fun(Expr, Bs_) ->
            {_, Result, Bs2} = expand(Forms, Expr, Bs_),
            {Result, Bs2}
        end, Bs_in, As0),

    case erl_eval2:match_list(H, As, erl_eval2:new_bindings()) of
        {match, Bsn} ->
            Bs1 = erl_eval2:add_bindings(Bsn, Bs0),
            case erl_eval2:guard(G, Bs1, Lf, Ef) of
                true ->
                    {ExprsV, Bs2} = expand_exprs(Forms, Exprs, Bs1),
                    {ok, ExprsV, Bs2};
                false -> expand_lfun(Forms, Cs, As, Bs0, Lf, Ef, RBs)
            end;
        nomatch ->
            expand_lfun(Forms, Cs, As, Bs0, Lf, Ef, RBs)
    end;
expand_lfun(_Forms, [], As, _Bs, _Lf, _Ef, _RBs) ->
    erlang:raise(error, function_clause,
        [{?MODULE, '-inside-an-interpreted-fun-', As}, {erl_eval2, expr, 3}]).

extract_fun(Name, Arity, Forms) ->
    case [F_ || {function, _, N_, A_, _Cs} = F_ <- Forms,
        N_ == Name, A_ == Arity] of
        [] ->
            erlang:error({undef, [{Name, Arity}]});
        [FForm] ->
            FForm
    end.

?funclog(":2").
eval_lfun({function, L, F, _, Clauses}, Args, Bs, Forms, Trace) ->
    {ArgsV, Bs1} = lists:mapfoldl(
        fun(A, Bs_) ->
            try
                begin
                    {value, AV, Bs1_} =
                        erl_eval2:expr(A, Bs_, lfh(Forms, Trace), nlf()),
                    {abstract(AV), Bs1_}
                end
            catch
                error:Err ->
                    io:format("eval-arg exception, arg:~p, bs:~p, err:~p~n", [A, Bs_, Err]),
                    exception_trace(lists:member(x, Trace), L, F, Args, Err),
                    {A, Bs_}
            end
        end, Bs, Args),

    Expr = {call, L, {'fun', L, {clauses, lfun_rewrite(Clauses, Forms)}}, ArgsV},
    try
        begin
            {value, Ret, _} =
                erl_eval2:expr(Expr, Bs, lfh(Forms, Trace), nlf()),
            ret_trace(lists:member(r, Trace) orelse lists:member(x, Trace),
                L, F, Args, Ret),
            %% restore bindings
            {value, Ret, Bs1}
        end
    catch
        error:Err ->
            io:format("eval-lfun exception, bs:~p err:~p~nexpr:~p~n", [Bs, Err, Expr]),
            exception_trace(lists:member(x, Trace), L, F, Args, Err),
            {value, Expr, Bs}
    end.

lfh(Forms, Trace) ->
    {eval, fun(Name, As, Bs1) ->
        io:format("in lfh, ~p, ~p~n", [Name, As]),
        eval_lfun(
            extract_fun(Name, length(As), Forms),
            As, Bs1, Forms, Trace)
           end}.

call_trace(false, _, _, _) -> ok;
call_trace(true, L, F, As) ->
    io:fwrite("ct_expand2 (~w): call ~s~n", [L, pp_function(F, As)]).

pp_function(F, []) ->
    atom_to_list(F) ++ "()";
pp_function(F, [A | As]) ->
    lists:flatten([atom_to_list(F), "(",
        [io_lib:fwrite("~w", [erl_parse:normalise(A)]) |
            [[",", io_lib:fwrite("~w", [erl_parse:normalise(A_)])] || A_ <- As]],
        ")"]).

ret_trace(false, _, _, _, _) -> ok;
ret_trace(true, L, F, Args, Res) ->
    io:fwrite("ct_expand2 (~w): returned from ~w/~w: ~w~n",
        [L, F, length(Args), Res]).

exception_trace(false, _, _, _, _) -> ok;
exception_trace(true, L, F, Args, Err) ->
    io:fwrite("ct_expand2 (~w): exception from ~w/~w: ~p~n", [L, F, length(Args), Err]).


lfun_rewrite(Exprs, Forms) ->
    Ret = parse_trans:plain_transform(
        fun({'fun', L, {function, F, A}}) ->
            {function, _, _, _, Cs} = extract_fun(F, A, Forms),
            {'fun', L, {clauses, Cs}};
%%            Cs;
            (_) ->
                continue
        end, Exprs),
%%    io:format("lfun-rewrite:~p~n", [Ret]),
    Ret.


%% abstract/1 - modified from erl_parse:abstract/1:
-type abstract_expr() :: term().
-spec abstract(Data) -> AbsTerm when
    Data :: term(),
    AbsTerm :: abstract_expr().
abstract(T) when is_function(T) ->
    case erlang:fun_info(T, module) of
        {module, erl_eval2} ->
            case erl_eval2:fun_data(T) of
                {fun_data, _Imports, Clauses} ->
                    {'fun', 0, {clauses, Clauses}};
                false ->
                    erlang:error(function_clause)  % mimicking erl_parse:abstract(T)
            end;
        _ ->
            erlang:error(function_clause)
    end;
abstract(T) when is_integer(T) -> {integer, 0, T};
abstract(T) when is_float(T) -> {float, 0, T};
abstract(T) when is_atom(T) -> {atom, 0, T};
abstract([]) -> {nil, 0};
abstract(B) when is_bitstring(B) ->
    {bin, 0, [abstract_byte(Byte, 0) || Byte <- bitstring_to_list(B)]};
abstract([C | T]) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C]);
abstract([H | T]) ->
    {cons, 0, abstract(H), abstract(T)};
abstract(Tuple) when is_tuple(Tuple) ->
%%    io:format("abstract, tuple:~p~n", [Tuple]),
    {tuple, 0, abstract_list(tuple_to_list(Tuple))}.

abstract_string([C | T], String) when is_integer(C), 0 =< C, C < 256 ->
    abstract_string(T, [C | String]);
abstract_string([], String) ->
    {string, 0, lists:reverse(String)};
abstract_string(T, String) ->
    not_string(String, abstract(T)).

not_string([C | T], Result) ->
    not_string(T, {cons, 0, {integer, 0, C}, Result});
not_string([], Result) ->
    Result.

abstract_list([H | T]) ->
    [abstract(H) | abstract_list(T)];
abstract_list([]) ->
    [].

abstract_byte(Byte, Line) when is_integer(Byte) ->
    {bin_element, Line, {integer, Line, Byte}, default, default};
abstract_byte(Bits, Line) ->
    Sz = bit_size(Bits),
    <<Val:Sz>> = Bits,
    {bin_element, Line, {integer, Line, Val}, {integer, Line, Sz}, default}.

