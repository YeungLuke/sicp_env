-module(env_display_test).

-export([test/0]).

-include("env_test.hrl").

test_display() ->
    env_run:run_file(?PATH ++ "display.scm").

test_sqrt_iter() ->
    env_run:run_file(?PATH ++ "sqrt_iter.scm").

test() ->
    test_display(),
    test_sqrt_iter(),
    ok.
