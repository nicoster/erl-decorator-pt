-module(func_logger).


-include("decorator_pt.hrl").
-compile([{parse_transform, decorator_pt_core}]).

-export([foo/2, funclogger/3]).

-define(funclog(), -decorate({?MODULE, funclogger, ?FUNCTION})).

funclogger(Func, Args, Data) ->
    fun() ->
        Indent = case erlang:get(func_logger) of undefined -> 0; Else -> Else end,
        io:format("~s-> ~p(~p)~n", [string:chars($ , Indent * 2), Data, Args]),
        erlang:put(func_logger, Indent + 1),
        Result = apply(Func, [Args]),
        erlang:put(func_logger, Indent),
        io:format("~s<- ~p:~p~n", [string:chars($ , Indent * 2), Data, Result]),
        Result
    end.

%%-decorate({?MODULE, funclogger, [?FUNCTION]}).
?funclog().
foo(Name, N) ->
    case N > 0 of
        true ->
%%        io:format("foo ~s ~n", [Name]),
            foo(Name, N - 1);
        _ -> ok
    end.