%%%-------------------------------------------------------------------
%%% @author nickx
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Mar 2017 3:07 PM
%%%-------------------------------------------------------------------
-author("nickx").

-define(export(), -export_func([])).
-compile({parse_transform, export}).
