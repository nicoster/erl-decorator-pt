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

-include("export.hrl").
-include("func_logger.hrl").

-compile({parse_transform, dump_asm}).

?export().
?funclog(":2, 4:").
%%?funclog("2:5").
foo(Name, A, B, C, E, N) ->
    case N > 0 of
        true ->
            foo(Name, A, B, C, E, N - 1);
        _ -> ok
    end.

?export().
bar() -> ok.
