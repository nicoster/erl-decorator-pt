-module(func_logger).

-compile([{parse_transform, decorator_pt_core}]).

-include("include/decorator_pt.hrl").

-export([foo/2, flogger/3]).


flogger(Func, Args, Data) ->
    % Selected =
    %     case length(Args) =:= 1 orelse proplists:get_bool(all, Data) of
    %         true -> Args;
    %         _ -> tl(Args)
    %     end,
    Indent = case erlang:get(func_logger) of undefined -> 0; Else -> Else end,
    io:format("~s-> ~p(~p)~n", [string:chars($ , Indent * 2), hd(Data), Args]),
    erlang:put(func_logger, Indent + 1),
    Result = apply(Func, [Args]),
    erlang:put(func_logger, Indent),
    io:format("~s<- ~p:~p~n", [string:chars($ , Indent * 2), hd(Data), Result]),
    Result.

 
-decorate({?MODULE, flogger, [bar]}).
foo(Name, N) ->
    case N > 0 of
        true ->
        io:format("foo ~p~n", [Name]),
        foo(Name, N - 1);
        _ -> ok
    end.