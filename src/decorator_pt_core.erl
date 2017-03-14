-module(decorator_pt_core).

-include("decorator_pt.hrl").

% adapted from: http://niki.code-karma.com/2011/06/python-style-decorators-in-erlang/
% changes:
%   - pass config options to the decorator
%   - wrap it in a macro (because bit ugly to use -decorate() direcly)

-export([parse_transform/2, pretty_print/1]).

%%--------------------------------------------------------------------
%% @doc Function gets called as a Parse Tranformation kickstarter
%%      TODO: add warnings for rogue decorators
-spec parse_transform(erl_parse:abstract_form(), compile:option()) -> erl_parse:abstract_form().
%% @end
%%--------------------------------------------------------------------
parse_transform(Ast,_Options)->
    {ExtendedAst2, RogueDecorators} = lists:mapfoldl(fun transform_node/2, [], Ast),

    Ast3 = lists:flatten(lists:filter(fun(Node)-> Node =/= nil end, ExtendedAst2))
        ++ emit_errors_for_rogue_decorators(RogueDecorators),

    Ast3.

%%--------------------------------------------------------------------
%% @doc Helper to pretty_print an AST.
-spec pretty_print(erl_parse:abstract_form()) -> string().
%% @end
%%--------------------------------------------------------------------
pretty_print(Ast) -> lists:flatten([erl_pp:form(N) || N<-Ast]).

emit_errors_for_rogue_decorators(DecoratorList)->
    [{error,{Line,erl_parse,["rogue decorator ", io_lib:format("~p",[D]) ]}} || {attribute, Line, decorate, D} <- DecoratorList].

%% @doc Rewrites the placeholders ?FUNCTION and ?ARITY to the current
%%      function name and arity.
%%
%% @spec parse_macro(any(), any(), erl_syntax:syntaxTree() ) ->
%%          erl_syntax:syntaxTree()
%% @end
parse_macro(FName, FArity, T) ->
%%    io:format("parse marco, T:~p~n", [T]),
    erl_syntax:revert(
        case erl_syntax:type(T) of
            atom ->
                case erl_syntax:atom_value(T) of
                    '__FUNCTION__'   -> erl_syntax:atom(FName);
                    '__ARITY__'      -> erl_syntax:integer(FArity);
                    _ -> T
                end;
            _ ->
                T
        end
    ).


% transforms module level nodes
% see http://www.erlang.org/doc/apps/erts/absform.html
% outputs nil (to swallow the node), a single node, or a list of nodes.
% nil nodes are removed in a subsequent pass and the lists flattened
transform_node(Node={attribute, _Line, decorate, _Decorator}, DecoratorList) when is_tuple(_Decorator) ->
    % keep a list of decorators but dont emit them in the code.
    % this is important as you arent meant to have attributes after functions in a module
    {nil, [Node|DecoratorList]};
transform_node({attribute, _Line, decorate, _Decorator}, DecoratorList) ->
    % this was an invalid decorator, so remove it entirely. Useful for testing
    {nil, DecoratorList};
transform_node(Node={function, _Line, _FuncName, _Arity, _Clauses}, []) ->
    % pass through decoratorless functions
    {Node, []};
transform_node(Node={function, _Line, _FuncName, _Arity, _Clauses}, DecoratorList) ->
    % apply decorators to this function and reset decorator list
    {apply_decorators(Node,DecoratorList), []};
transform_node(Node={eof,_Line}, DecoratorList) ->
    {[Node| emit_errors_for_rogue_decorators(DecoratorList) ], []};
transform_node(Node, DecoratorList) ->
    % some other form (only other valid forms are other attributes)
    % keep going
    {Node, DecoratorList}.

apply_decorators(Node={function, Line, FuncName, Arity, Clauses}, DecoratorList) when length(DecoratorList) > 0 ->
    % Functions can have multiple function clauses (eg when using guards)
    [{clause,_, _Args,_, _}|_] = Clauses,

    [
        % output the original function renamed
        function_form_original(Node),
        % output a trampoline into our decorator chain
        function_form_trampoline(Node, DecoratorList),
        % output the funname_arityn_0 function to unpack the arg list and forward to the original
        function_form_unpacker(Line,FuncName,Arity)
        % output our decorator chain
        | function_forms_decorator_chain(Line, FuncName, Arity, DecoratorList)
    ].

function_form_original({function, Line, FuncName, Arity, Clauses}) ->
    {function, Line, generated_func_name({original,FuncName}), Arity, Clauses}.

% outputs a single clause function that gets the first decorator chain function and calls it
function_form_trampoline({function, Line, FuncName, Arity, Clauses}, DecoratorList) ->
    [{clause,_, _Args,_, _}|_] = Clauses,
    NumDecorators = length(DecoratorList),
    ArgNames = arg_names(Arity),
    { function, Line, FuncName, Arity, [{
        clause, Line,
        %Args,
        emit_arguments(Line, ArgNames),
        emit_guards(Line, []),
        [
            emit_local_call( Line,
                generated_func_name({decorator_wrapper, FuncName, Arity, NumDecorators}),
                [emit_atom_list(Line, ArgNames)]
            )
        ]
    }]}.

function_form_unpacker(Line,FuncName,Arity) ->
    ArgNames = arg_names(Arity),
    OriginalFunc = generated_func_name({original,FuncName}),
    { function, Line,
        generated_func_name({decorator_wrapper, FuncName, Arity, 0}), 1, [{
            clause, Line,
            [emit_atom_list(Line, ArgNames)],
            emit_guards(Line, []),[
                {call,Line, {atom,Line,OriginalFunc},
                    emit_arguments(Line,ArgNames)}
            ]
    }]}.

function_forms_decorator_chain(Line, FuncName, Arity, DecoratorList) ->
    NumDecorators = length(DecoratorList),
    DecoratorIndexes = lists:zip(DecoratorList, lists:seq(1, NumDecorators)),
    [ function_form_decorator_chain(Line,FuncName,Arity,DecoAttrs,Pos)
        || { {attribute,_,decorate, DecoAttrs},Pos} <- DecoratorIndexes ] .


function_form_decorator_chain(Line,FuncName,Arity, DecoAttrs, DecoratorIndex) ->
    NextFuncName = generated_func_name({decorator_wrapper, FuncName, Arity, DecoratorIndex-1}),
    {function, Line,
        generated_func_name({decorator_wrapper, FuncName,Arity, DecoratorIndex}), % name
        1, % arity
        [{ clause, Line,
            emit_arguments(Line, ['ArgList'] ),
            emit_guards(Line, []),
            [
                % F = DecMod:DecFun( fun NextFun/1, ArgList),
                emit_decorated_fun(Line, FuncName, Arity, 'F', DecoAttrs,  NextFuncName, 'ArgList'),
                % call 'F'
                {call, Line,{var,Line,'F'},[]}
            ]
        }]
    }.

emit_decorated_fun(Line, OriginalFunction, Arity, Name, {DecFun, DecExtraArgs}, InnerFunName, ArgName)->
    {match,Line,
        {var,Line,Name},
        {call,Line,
            {atom,Line,DecFun},
%%            {remote, Line, {atom,Line,DecMod},{atom,Line,DecFun}},
            [
                {'fun',Line,{function, InnerFunName, 1}},
                {var, Line, ArgName},
                {call,Line,
                    {remote,Line,{atom,Line,ct_expand},{atom,Line,term}},
                    [{call,Line,{atom,Line,preprocess_data},[
                        erl_syntax_lib:map(fun(TE) -> parse_macro(OriginalFunction, Arity, TE) end, erl_syntax:abstract(DecExtraArgs))
                    ]}]}
            ]
       }
    }.

emit_local_call(Line, FuncName, ArgList) ->
    {call, Line, {atom, Line, FuncName}, ArgList}.

emit_arguments(Line, AtomList) ->
    [{var,Line,Arg} || Arg <- AtomList].

emit_guards(_Line, [])->
    [].

emit_atom_list(Line, AtomList) ->
    % build a list of args out of cons cells
    % {cons,43,{var,43,'Arg1'},{cons,43,{var,43,'Arg2'},{nil,43}}}
    lists:foldr(fun(Arg, Acc) ->
        {cons, Line, {var, Line, Arg}, Acc}
    end, {nil,Line}, AtomList).

generated_func_name( {original, OrigName} ) ->
    atom_name([OrigName, "_original___"]);
%generated_func_name( {trampoline, OrigName} ) ->
%    OrigName;
generated_func_name( {decorator_wrapper, OrigName, Arity, N} ) ->
    atom_name([OrigName, "_arity", Arity, "_", N]).

% list() -> atom()
atom_name(Elements) ->
    list_to_atom(lists:flatten(lists:map(
        fun
            (A) when is_atom(A) -> atom_to_list(A);
            (A) when is_number(A) -> io_lib:format("~p",[A]);
            (A) when is_binary(A) -> io_lib:format("~s",[A]);
            (A) when is_list(A) -> io_lib:format("~s",[A])
        end,
        Elements
    ))).

arg_names(Arity) ->
    [ atom_name(["Arg", ArgNum]) || ArgNum <- lists:seq(1,Arity) ].
