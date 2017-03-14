
%%-compile({parse_transform, dump_asm}).

-ifdef(DEBUG).
-compile([{parse_transform, decorator_pt_core}]).
-compile({parse_transform, ct_expand}).
-compile(nowarn_unused_function).
-define(funclog(ArgLogSpans), -decorate({funclogger, ['__FUNCTION__', '__ARITY__', ArgLogSpans]})).
-else.
-define(funclog(Spans), -compile([])).
-endif.

%%-compile({parse_transform, dump_asm}).

preprocess_data([Func, Arity, ArgLogSpans] = Args) ->
    io:format("func:~p arity:~p scope:~p~n", Args),
    ArgSpans = [X || X <- ArgLogSpans, X =/= $ ],
    PosList = lists:flatten(
        [case binary:split(Span, <<":">>) of
             [<<>>, <<>>] -> lists:seq(0, Arity - 1);
             [Start, <<>>] -> lists:seq(binary_to_integer(Start), Arity - 1);
             [<<>>, End] -> lists:seq(0, binary_to_integer(End) rem Arity);
             [Start, End] -> lists:seq(binary_to_integer(Start), binary_to_integer(End));
             _ -> throw({error, 0, preprocess_data})
         end || Span <- binary:split(list_to_binary(ArgSpans), <<",">>, [global, trim])]),
    UniqueList = lists:sort(sets:to_list(sets:from_list(PosList))),
    ArgFormats = lists:flatten(join(["~p" || _ <- lists:seq(1, length(UniqueList))], ", ")),
    Format = lists:flatten(io_lib:format("~~s-> ~~p(~s) ~~n", [ArgFormats])),
    {Func, Format, UniqueList}.

-spec join(iolist(), Sep :: term()) -> iolist().
join([Head | Tail], Sep) ->
    join_list_sep(Tail, Sep, [Head]);
join([], _Sep) ->
    [].

-spec join_list_sep(iolist(), Sep :: term(), Acc :: iolist()) -> iolist().
join_list_sep([Head | Tail], Sep, Acc) ->
    join_list_sep(Tail, Sep, [Head, Sep | Acc]);
join_list_sep([], _Sep, Acc) ->
    lists:reverse(Acc).

funclogger(FuncWrapper, Args, {FuncName, Format, PosList}) ->
    fun() ->
        Indent = case erlang:get(func_logger) of undefined -> 0; Else -> Else end,
        io:format(Format, [string:chars($ , Indent * 2), FuncName | [lists:nth(Pos + 1, Args) || Pos <- PosList]]),
        erlang:put(func_logger, Indent + 1),
        Result = apply(FuncWrapper, [Args]),
        erlang:put(func_logger, Indent),
        io:format("~s<- ~p:~p~n", [string:chars($ , Indent * 2), FuncName, Result]),
        Result
    end.

