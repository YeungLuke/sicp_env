-module(env).
-export([init_global_env/1, stop_envs/1]).
-export([lookup/2, set/3, define/3, extend_env/3, mark_remove_env/3,
         force_thunk/2, new_thunk/2]).

% enviroment
loop(Frame, Parent) ->
    receive
        {lookup, From, Key} ->
            case {lists:keyfind(Key, 1, Frame), Parent} of
                {false, none} ->
                    From ! {error, none};
                {false, Parent} ->
                    From ! lookup_inner(Key, Parent);
                {{Key, Value}, _} ->
                    From ! {ok, Value}
            end,
            loop(Frame, Parent);
        {define, From, Key, Value} ->
            case lists: keyfind (Key, 1, Frame) of
                false ->
                    NewFrame = [{Key, Value} | Frame],
                    From ! ok;
                _ ->
                    % NewFrame = Frame,
                    % From ! error
                    NewFrame = lists:keyreplace(Key, 1, Frame, {Key, Value}),
                    From ! ok
            end,
            loop(NewFrame, Parent);
        {set, From, Key, Value} ->
            case {lists:keyfind (Key, 1, Frame), Parent} of
                {false, none} ->
                    NewFrame = Frame,
                    From ! error;
                {false, Parent} ->
                    NewFrame = Frame,
                    From ! set_inner(Key, Value, Parent);
                _ ->
                    NewFrame = lists:keyreplace(Key, 1, Frame, {Key, Value}),
                    From ! ok
            end,
            loop(NewFrame, Parent)
    end.

lookup_inner(Key, EnvPid) ->
    EnvPid ! {lookup, self(), Key},
    receive
        Msg -> Msg
    end.

set_inner(Key, Value, EnvPid) ->
    EnvPid ! {set, self(), Key, Value},
    receive
        Msg -> Msg
    end.

lookup(Key, EnvPid) ->
    EnvPid ! {lookup, self(), Key},
    receive
        {ok, Value} ->
            Value;
        {error, _} ->
            error("lookup error", [Key])
    end.

set(Key, Value, EnvPid) ->
    EnvPid ! {set, self(), Key, Value},
    receive
        ok ->
            ok;
        error ->
            error("set error", [Key])
    end.

define(Key, Value, EnvPid) ->
    EnvPid ! {define, self(), Key, Value},
    receive
        ok ->
            ok;
        error ->
            error("define error", [Key])
    end.

init_global_env(Frame) ->
    spawn(fun() -> loop(Frame, none) end).

stop_envs(_) ->
    stop.

extend_env(Parameters, Arguments, Env) ->
    spawn(fun() -> loop(lists:zip(Parameters, Arguments), Env) end).

mark_remove_env(_, _, _) ->
    ok.

thunk_loop(Exp, Env) ->
    receive
        {force, From} ->
            Result = envmodel:actual_value(Exp, Env),
            From ! {self(), Result},
            thunk_loop(Result)
    end.

thunk_loop(Result) ->
    receive
        {force, From} ->
            From ! {self(), Result},
            thunk_loop(Result)
    end.

force_thunk(Pid, _Env) ->
    Pid ! {force, self()},
    receive
        {Pid, Msg} -> Msg
    end.

new_thunk(Exp, Env) ->
    spawn(fun() -> thunk_loop(Exp, Env) end).
