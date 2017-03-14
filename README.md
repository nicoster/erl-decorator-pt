Purpose
=======

This application implements a set of transformations in the erlang syntax tree allowing the
following:

* The *decorator* directive on top of functions in order to add extra functionality to them
* The *funclog* macro on top of functions to log function entries/exits.
* The *export_func* directive on top of functions to export them

Usage
=====

## -decorate

There are many ways to include these macros in your projects. Just remember macros and macro
debugging in particular are not the simplest tools in the world. _A great power comes with a great
responsibility._

The most common way of using this application is:

1. Make it available to your application via rebar3

2. Create an .hrl file loading hooking the decorator_pt_core in the compilation process and defining
   a macro with a representative name for your decorator. i.e. my_decorator.hrl

   ```
   -compile([{parse_transform, decorator_pt_core}]).

   -define(MY_DECORATOR(Options), -decorate({MyCbFun, {?MODULE, ?FUNCTION, Options}})).

   ```
3. Define the callback function in the callback module to your liking. Keep in mind what the
   signature of your callback should be:

   ```
   my_cb_fun(OriginalFun::function(), Args::[term()],
             {OriginalModule::atom(), OriginalFunctionName::atom(), Opts::[term()]) ->
       FunReturningResult::fun(() -> term()).
   ```

4. When using your decorator, include *my_decorator.hrl* and *decorator_pt.hrl* in that order.

   ```
   -include_lib("my_app/include/my_decorator.hrl").
   -include_lib("decorator_pt/include/decorator_pt.hrl").

   [...]

   ?MY_DECORATOR([{opt1, val}]).
   my_decorated_fun() ->
       ok.

   ```

## ?funclog(":")
1. include func_logger.hrl in your source file
   ```
   -include("func_logger.hrl").
   ```
2. put ?funclog(":") on the top of the function you need to inspect the arguments/return value.
   ```
   ?funclog(":").
   foo(Name, A, B, C, E, N) ->
       case N > 0 of
           true ->
               foo(Name, A, B, C, E, N - 1);
           _ -> ok
       end.
   ```

## -export_func
1. include export.hrl in your source file. make sure export.hrl is on the top of the include list. this is to prevent other parse_transforms (like func_logger) to tamper the function names.

   ```
   -include("export.hrl").
   ```

2. put ?export() on the top of the function you want to export.

   ```
   ?export().
   bar() -> ok.
   ```
