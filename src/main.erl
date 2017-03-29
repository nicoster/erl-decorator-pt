%%%-------------------------------------------------------------------
%%% @author nickx
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Mar 2017 11:23 PM
%%%-------------------------------------------------------------------
-module(main).
-author("nickx").

%% API

%%-define(DEBUG, true).

%%-include("export.hrl").
%%-include("func_logger.hrl").
%%-compile({ct_expand_trace, [c]}).

%%-compile({parse_transform, dump_ast}).
-compile({parse_transform, ct_expand2}).
%%-compile({parse_transform, cut}).
-compile({parse_transform, dump_ast}).

%%?export().
%%?funclog(":2, 4:").
%%?funclog("2:5").
%%foo(Name, A, B, C, E, N) ->
%%    case N > 0 of
%%        true ->
%%            foo(Name, A, B, C, E, N - 1);
%%        _ -> ok
%%    end.

get_props(Type) ->
    case Type of
        "a" -> 3;
        "b" -> 5
    end.


get_length(Type, U) ->
    Typo = atom_to_list(Type),
    case get_props(Typo) of
        3 -> length(U) + 1;
        5 ->
            Binary = list_to_binary(U),
            size(Binary)
    end.

%%foo() -> fun(Type) -> get_length(Type, "234") end (a).
%%bar() -> get_length(a, _).

%%?export().
get_length_a(U) -> ct_expand2:term(get_length(a, U)). %% length(U) + 1

get_length_b(U) -> ct_expand2:term(get_length(b, U)).