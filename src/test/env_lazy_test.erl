-module(env_lazy_test).

-include("env_test.hrl").

lazy_try_test() ->
    ?assertError(badarith, env_run:run_file(?PATH ++ "lazy_try_error.scm")),
    ?assertEqual({num_val, 1}, env_run:run_file(?PATH ++ "lazy_try.scm")).

lazy_param_test() ->
    ?assertEqual({num_val, 4}, env_run:run_file(?PATH ++ "lazy_param.scm")).

lazy_stream_test() ->
    Run = env_run:run_fun([format, true]),

    Run({file, ?PATH ++ "lazy_stream.scm"}),
    ?assertEqual("18", Run("(list-ref integers 17)")),
    ?assertEqual("2.716923932235896", Run("(list-ref (solve (lambda (x) x) 1 0.001) 1000)")),

    Run(stop).
