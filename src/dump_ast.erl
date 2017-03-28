%%%-------------------------------------------------------------------
%%% @author nickx
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Mar 2017 11:35 PM
%%%-------------------------------------------------------------------
-module(dump_ast).
-author("nickx").

%% API
-export([parse_transform/2]).

parse_transform(Ast,_Options)->
    io:format("= dump-ast ============================================================~n~p",[Ast]),
    io:format("~n= pp =================================================================~n~s",[pretty_print(Ast)]),
    Ast.

pretty_print(Ast) -> lists:flatten([erl_pp:form(N) || N<-Ast]).
