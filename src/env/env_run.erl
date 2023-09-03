-module(env_run).

-export([run_fun/1, run_fun/0, run_code/1, run_code/2, run_file/1]).

-include("env.hrl").

run_one(Code, Env) ->
    % old code
    % Token = parse:parse(Code),
    % Prog = make_exp:make_exp(Token),
    {ok, Tokens, _} = lex:string(Code),
    {ok, Prog} = grm:parse(Tokens),
    % io:format("prog: ~p~n", [Prog]),

    envmodel:actual_value(Prog, Env).

run_one_file(Path, Env) ->
    {ok, Bin} = file:read_file(Path),
    run_one(binary_to_list(Bin), Env).

global_env() ->
    ?ENV_MODULE:init_global_env(global_frame:global_frame()).

format(Res, Format) ->
    case Format of
        true ->
            global_frame:to_string(Res);
        _ ->
            Res
    end.

run_fun(Opt) ->
    Format = proplists:get_value(format, Opt),
    GlobalEnv = global_env(),
    fun(stop) -> ?ENV_MODULE:stop_envs(GlobalEnv);
       ({file, Path}) ->
            format(run_one_file(Path, GlobalEnv), Format);
       (Code) -> format(run_one(Code, GlobalEnv), Format)
    end.

run_fun() ->
    run_fun([]).

run_code(Code, Opt) ->
    Run = run_fun(Opt),
    Res = Run(Code),
    Run(stop),
    Res.

run_code(Code) ->
    run_code(Code, []).

run_file(Path) ->
    {ok, Bin} = file:read_file(Path),
    run_code(binary_to_list(Bin)).
