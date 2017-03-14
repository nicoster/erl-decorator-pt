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

transform_node({attribute, _Line, export_func, _}, {none, Exports}) ->
    {nil, {export_func, Exports}};
transform_node(Node={function, _Line, _FuncName, _Arity, _Clauses}, {none, Exports})->
    {Node, {none, Exports}};
transform_node(Node={function, _Line, FuncName, Arity, _Clauses}, {export_func, Exports}) ->
    {Node, {none, [{FuncName, Arity} | Exports]}};
transform_node(Node={eof,_Line}, {_, Exports}) ->
    {Node, Exports};
transform_node(Node, Exports) ->
    {Node, Exports}.