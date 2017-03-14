%%%-------------------------------------------------------------------
%%% @author nickx
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Mar 2017 11:36 PM
%%%-------------------------------------------------------------------
-module(export).
-author("nickx").

%% API
-export([parse_transform/2]).


parse_transform(Ast,_Options)->
    {Ast2, Exports} = lists:mapfoldl(fun transform_node/2, {none, []}, Ast),
    lists:reverse(lists:foldl(
        fun(Node, Acc) ->
            case Node of
                {attribute, Line, module, _} -> [{attribute, Line, export, Exports}, Node | Acc];
                nil -> Acc;
                _ -> [Node | Acc]
            end
        end, [], Ast2
    )).

% transforms module level nodes
% see http://www.erlang.org/doc/apps/erts/absform.html
% outputs nil (to swallow the node), a single node, or a list of nodes.
% nil nodes are removed in a subsequent pass and the lists flattened
transform_node({attribute, _Line, export_func, _}, {none, Exports}) ->
    % keep a list of decorators but dont emit them in the code.
    % this is important as you arent meant to have attributes after functions in a module
    {nil, {export_func, Exports}};
transform_node(Node={function, _Line, _FuncName, _Arity, _Clauses}, {none, Exports})->
    {Node, {none, Exports}};
transform_node(Node={function, _Line, FuncName, Arity, _Clauses}, {export_func, Exports}) ->
    {Node, {none, [{FuncName, Arity} | Exports]}};
transform_node(Node={eof,_Line}, {_, Exports}) ->
    {Node, Exports};
transform_node(Node, Exports) ->
    % some other form (only other valid forms are other attributes)
    % keep going
    {Node, Exports}.