-module(env_gc).

-export([init_global_env/1, stop_envs/1]).
-export([lookup/2, set/3, define/3, extend_env/3, mark_remove_env/3,
         force_thunk/2, new_thunk/2]).

-define(MIN_GC, 256).
-define(GC_SIZE, 1024 * 1024).

-record(env_state, {envs = #{},
                    env_num,
                    last_gc = ?MIN_GC,
                    global,
                    thunks = #{},
                    thunk_num = 0}).

env_lookup(Envs, Env, Key) ->
    case maps:find(Env, Envs) of
        {ok, {Frame, Parent, _}} ->
            case lists:keyfind(Key, 1, Frame) of
                false ->
                    env_lookup(Envs, Parent, Key);
                {Key, Value} ->
                    {ok, Value}
            end;
        _ ->
            % io:format("env_Lookup find key ~p env ~p error~n~p~n", [Key, Env, Envs]),
            {error, none}
    end.

env_define(Envs, Env, Key, Value) ->
    case maps:find(Env, Envs) of
        {ok, {Frame, Parent, RE}} ->
            case lists:keyfind(Key, 1, Frame) of
                false ->
                    {ok, {[{Key, Value} | Frame], Parent, RE}};
                _ ->
                    {ok, {lists:keyreplace(Key, 1, Frame, {Key, Value}), Parent, RE}}
            end;
        _ ->
            {error, none}
end.

env_set(Envs, Env, Key, Value) ->
    case maps:find(Env, Envs) of
        {ok, {Frame, Parent, RE}} ->
            case lists:keyfind(Key, 1, Frame) of
                false ->
                    env_set(Envs, Parent, Key, Value);
                _ ->
                    {ok, Env, {lists:keyreplace(Key, 1, Frame, {Key, Value}), Parent, RE}}
            end;
        _ ->
            {error, none, none}
    end.

env_gc_value(Envs, {thunk_memo, ThunkId, {Env, _}}, NewEnvs, Thunks, NewThunks) ->
    NewThunks1 = NewThunks#{ThunkId => maps:get(ThunkId, Thunks)},
    case maps:is_key(Env, NewEnvs) of
        true ->
            {NewEnvs, NewThunks1};
        _ ->
            env_gc_env(Envs, Env, Thunks, NewEnvs, NewThunks1)
    end;
env_gc_value(Envs, {thunk, _Exp, {Env, _}}, NewEnvs, Thunks, NewThunks) ->
    case maps:is_key(Env, NewEnvs) of
        true ->
            {NewEnvs, NewThunks};
        _ ->
            env_gc_env(Envs, Env, Thunks, NewEnvs, NewThunks)
    end;
env_gc_value(Envs, {procedure, _, _, {Env, _EnvPid}}, NewEnvs, Thunks, NewThunks) ->
    case maps:is_key(Env, NewEnvs) of
        true ->
            {NewEnvs, NewThunks};
        _ ->
            env_gc_env(Envs, Env, Thunks, NewEnvs, NewThunks)
    end;
    % NewEnvs#{Env => maps:get(Env, Envs)};
env_gc_value(Envs, {pair_val, [H|T]}, NewEnvs, Thunks, NewThunks) ->
    {NewEnvs1, NewThunks1} = env_gc_value(Envs, H, NewEnvs, Thunks, NewThunks),
    env_gc_value(Envs, T, NewEnvs1, Thunks, NewThunks1);
env_gc_value(_, _, NewEnvs, _, NewThunks) ->
    {NewEnvs, NewThunks}.

env_gc_frame(_, [], NewEnvs, _, NewThunks) ->
    {NewEnvs, NewThunks};
env_gc_frame(Envs, [{_, Value} | T], NewEnvs, Thunks, NewThunks) ->
    {NewEnvs1, NewThunks1} = env_gc_value(Envs, Value, NewEnvs, Thunks, NewThunks),
    env_gc_frame(Envs, T, NewEnvs1, Thunks, NewThunks1).

env_gc_frame_thunk([], _Thunks, Frame) ->
    Frame;
env_gc_frame_thunk([{K, {thunk_memo, ThunkId, _Env} = V} | T], Thunks, Frame) ->
    NewV =
    case maps:get(ThunkId, Thunks) of
        {thunk, _, _} -> V;
        {thunk_memo, Result, _} -> Result
    end,
    env_gc_frame_thunk(T, Thunks, [{K, NewV} | Frame]);
env_gc_frame_thunk([H | T], Thunks, Frame) ->
    env_gc_frame_thunk(T, Thunks, [H | Frame]).

env_gc_env(Envs, Env, Thunks, NewEnvs, NewThunks) -> 
    case maps:find(Env, Envs) of
        {ok, {Frame, Parent, RE}} ->
            NewFrame = env_gc_frame_thunk(Frame, Thunks, []),
            NewEnvs1 = NewEnvs#{Env => {NewFrame, Parent, RE}},
            {NewEnvs2, NewThunks2} = env_gc_frame(Envs, NewFrame, NewEnvs1, Thunks, NewThunks),
            env_gc_env(Envs, Parent, Thunks, NewEnvs2, NewThunks2);
        _ ->
            {NewEnvs, NewThunks}
end.

env_gc_marked_envs(Envs, NewEnvs, Thunks, NewThunks) ->
    maps:fold(fun(_ , {_, _, true}, In) -> In;
                 (Env, _, {EnvsIn, ThunksIn}) -> env_gc_env(Envs, Env, Thunks, EnvsIn, ThunksIn)
              end, {NewEnvs, NewThunks}, Envs).

need_gc(Envs, Last) ->
    if
        Last < ?GC_SIZE -> maps:size(Envs) > Last * 2;
        true -> maps:size(Envs) > (Last + ?GC_SIZE)
    end.
    % false.
    % true.

env_gc(Envs, NewEnv, Last, Thunks) ->
    case need_gc(Envs, Last) of
        true ->
            {NewEnvs1, NewThunks1} = env_gc_env(Envs, NewEnv, Thunks, #{}, #{}),
            {NewEnvs, NewThunks} = env_gc_marked_envs(Envs, NewEnvs1, Thunks, NewThunks1),
            if
                NewEnvs =/= Envs -> 
                    % io:format("before gc~n~p~n", [maps:remove(1, Envs)]),
                    % io:format("after gc ~p~n~p~n", [Envs, maps:remove(1, NewEnvs)]),
                    io:format("gc before ~p after ~p~n", [maps:size(Envs), maps:size(NewEnvs)]),
                    ok;
                true ->
                    ok
            end,
            {true, NewEnvs, NewThunks};
        _ ->
            {false, Envs, Thunks}
    end.

env_extend(Envs, Env, Frame, Parent) ->
    Envs#{Env => {Frame, Parent, false}}.

env_mark_remove(Envs, Env) ->
    case maps:find(Env, Envs) of
        {ok, {Frame, Parent, _}} ->
            {Frame, Parent, true};
        _ ->
            error
    end.

env_mark_result(Envs, {procedure, _, _, {Env, _EnvPid}}) ->
    case maps:find(Env, Envs) of
        {ok, {Frame, Parent, _}} ->
            Envs#{Env => {Frame, Parent, false}};
        _ ->
            Envs
    end;
env_mark_result(Envs, _) ->
    Envs.

% env_mark_arg(Envs, {procedure, _, _, {Env, _EnvPid}}) ->
%     case maps:find(Env, Envs) of
%         {ok, {Frame, Parent, _}} ->
%             Envs#{Env => {Frame, Parent, true}};
%         _ ->
%             Envs
%     end;
env_mark_arg(Envs, _) ->
    Envs.

env_mark_args(Envs, []) ->
    Envs;
env_mark_args(Envs, [H|T]) ->
    env_mark_args(env_mark_arg(Envs, H), T).

do_force_thunk(Thunks, Thunk) ->
    case maps:find(Thunk, Thunks) of
        {ok, ThunkInfo} ->
            ThunkInfo;
        _ ->
            error
    end.

loop(S=#env_state{envs=Envs, env_num=EnvNum, last_gc=Last, thunks=Thunks, thunk_num=TNum}) ->
    receive
        {lookup, From, Env, Key} ->
            From ! env_lookup(Envs, Env, Key),
            loop(S) ;
        {define, From, Env, Key, Value} ->
            {Rsp, EnvInfo} = env_define(Envs, Env, Key, Value),
            From ! Rsp,
            case Rsp of
                error ->
                    loop(S);
                ok ->
                    loop(S#env_state{envs=Envs#{Env => EnvInfo}})
            end;
        {set, From, Env, Key, Value} ->
            {Rsp, SetEnv, EnvInfo} = env_set(Envs, Env, Key, Value),
            From ! Rsp,
            case Rsp of
                error ->
                    loop(S);
                ok ->
                    loop(S#env_state{envs=Envs#{SetEnv => EnvInfo}})
            end;
        {extend_env, From, Frame, Parent} ->
            % io:format("extend_env Parent: ~p, EnvNum ~p~n", [Parent, EnvNum]),
            case maps:is_key(Parent, Envs) of
                true ->
                    NewNum = EnvNum + 1,
                    From ! {ok, NewNum},
                    ExtendEnvs = env_extend(Envs, NewNum, Frame, Parent),
                    {GC, NewEnvs, NewThunks} = env_gc(ExtendEnvs, NewNum, Last, Thunks),
                    loop(S#env_state{envs=NewEnvs,
                                     env_num=NewNum,
                                     last_gc=case GC of true -> max(maps:size(NewEnvs), ?MIN_GC); _ -> Last end,
                                     thunks=NewThunks}) ;
                false ->
                    % io:format("extend_env error ~p ~p~n", [EnvNum, Envs]),
                    From ! error,
                    loop(S)
            end;
        {mark_remove, Env, Result, ArgValues} ->
            % io:format("mark_remove ~p~n", [Env]),
            EnvInfo = env_mark_remove(Envs, Env),
            NewEnvs = env_mark_args(Envs#{Env => EnvInfo}, ArgValues),
            loop(S#env_state{envs=env_mark_result(NewEnvs, Result)});
        {new_thunk, EnvInfo, Exp, From} ->
            NewNum = TNum + 1,
            From ! {ok, NewNum},
            loop(S#env_state{thunks=Thunks#{NewNum => {thunk, Exp, EnvInfo}},
                             thunk_num=NewNum});
        {get_thunk, Thunk, From} ->
            Result = do_force_thunk(Thunks, Thunk),
            From ! Result,
            loop(S);
        {force_thunk, Thunk, Result, EnvInfo} ->
            loop(S#env_state{thunks=Thunks#{Thunk => {thunk_memo, Result, EnvInfo}}});
        stop -> 
            % io:format("envs:~n~p~n", [maps:remove(1, Envs)]),
            io:format("env stopping, env size is ~p, thunk size is ~p~n", [maps:size(Envs), maps:size(Thunks)]),
            % {_, NewEnvs, NewThunks} = env_gc(Envs, 1, Last, Thunks),
            % io:format("gced envs:~n~p~n", [maps:without(maps:keys(NewEnvs), Envs)]),
            % io:format("env stopped, env size is ~p, thunk size is ~p~n", [maps:size(NewEnvs), maps:size(NewThunks)]),
            ok
    end.

lookup(Key, {Env, EnvPid}) ->
    EnvPid ! {lookup, self(), Env, Key},
    receive
        {ok, Value} ->
            Value;
        {error, _} ->
            error("lookup error", [Key, Env, EnvPid])
        % after 5000 ->
        %     error("lookup timeout", [Key, EnvPid])
    end.

set(Key, Value, {Env, EnvPid}) ->
    EnvPid ! {set, self(), Env, Key, Value},
    receive
        ok ->
            ok;
        error ->
            error("set error", [Key, Env, EnvPid])
        % after 5000 ->
        %     error("set timeout", [Key, Value, EnvPid])
    end.

define(Key, Value, {Env, EnvPid}) ->
    EnvPid ! {define, self(), Env, Key, Value},
    receive
        ok ->
            ok;
        error ->
            error("define error", [Key, Env, EnvPid])
        % after 5000 ->
        %     error("define timeout", [Key, Value, EnvPid])
    end.

init_global_env(Frame) ->
    Ref = 1,
    {Ref, spawn(fun() -> loop(#env_state{envs = #{Ref => {Frame, none, false}},
                                         env_num = Ref,
                                         global = Ref}) end)}.

% make_list([], _, Nil) ->
%     Nil;
% make_list([H|T], Cons, Nil) ->
%     applies(Cons, [H, make_list(T, Cons, Nil)]).

get_frame([], [], _, Frame) ->
    Frame;
get_frame(['.', Last], Remain, _Env, Frame) ->
    % Frame ++ [{Last, make_list(Remain,
    %                            lookup('cons', Env),
    %                            lookup('nil', Env))}];
    Frame ++ [{Last, global_frame:make_list(Remain)}];
get_frame([P|PT], [A|AT], Env, Frame) ->
    get_frame(PT, AT, Env, Frame ++ [{P, A}]).

extend_env(Parameters, Arguments, {Env, EnvPid} = EnvInfo) ->
    % Frame = lists:zip(Parameters, Arguments),
    Frame = get_frame(Parameters, Arguments, EnvInfo, []),
    EnvPid ! {extend_env, self(), Frame, Env},
    receive
        {ok, Ref} ->
            {Ref, EnvPid};
        error ->
            error("extend_env error", [Env, Parameters, Arguments, self()])
        % after 5000 ->
        %     error("extend_env timeout", [Env, Parameters, Arguments, self()])
    end.

stop_envs({_Env, EnvPid}) ->
    EnvPid ! stop.

mark_remove_env({Env, EnvPid}, Result, ArgValues) ->
    EnvPid ! {mark_remove, Env, Result, ArgValues}.

new_thunk(Exp, {_Env, EnvPid}=EnvInfo) ->
    EnvPid ! {new_thunk, EnvInfo, Exp, self()},
    % io:format("new_thunk~n"),
    receive
        {ok, Thunk} ->
            % io:format("new_thunk ~p~n", [Thunk]),
            Thunk
    end.

force_thunk(Thunk, {_Env, EnvPid}) ->
    EnvPid ! {get_thunk, Thunk, self()},
    % io:format("get_thunk, Thunk ~p~n", [Thunk]),
    receive
        {thunk, Exp, EnvInfo} ->
            % io:format("thunk, got ~p~n", [Thunk]),
            Result = envmodel:actual_value(Exp, EnvInfo),
            EnvPid ! {force_thunk, Thunk, Result, EnvInfo},
            Result;
        {thunk_memo, Result, _EnvInfo} ->
            % io:format("thunk_memo, got ~p~n", [Thunk]),
            Result
    end.
