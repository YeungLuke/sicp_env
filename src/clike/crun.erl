-module(crun).

-export([test/0, code/1]).

-include("env.hrl").

global_env() ->
    ?ENV_MODULE:init_global_env(global_frame:global_frame()).

code(Code) ->
    Env = global_env(),
    {ok, Tokens, _} = clex:string(Code),
    {ok, Prog} = cgrm:parse(Tokens),
    % io:format("prog: ~p~n", [Prog]),

    envmodel:actual_value(Prog, Env).

test() ->
    {num_val, 3} = code("1+2;"),

    ok.
