-module(envmodel_test).

-include("env.hrl").
-include("env_test.hrl").

counter_test() ->
    GlobalEnv = ?ENV_MODULE:init_global_env([{'+', {binf, fun({num_val, X}, {num_val, Y}) -> {num_val, X + Y} end}}]),
    Prog = [{define,make_counter,
                {lambda,[n],
                    [{lambda,[],
                        [{assign,n,{apply,{var_exp,'+'},[{var_exp,n},{num_val,1}]}},
                        {var_exp,n}]}]}},
            {define,c1,{apply,{var_exp,make_counter},[{num_val,0}]}},
            {define,c2,{apply,{var_exp,make_counter},[{num_val,10}]}},
            {apply,{var_exp,c1},[]},
            {apply,{var_exp,c2},[]}],
    
    envmodel:actual_value(Prog, GlobalEnv),

    {procedure, _, _, C1Env} = ?ENV_MODULE:lookup(c1, GlobalEnv),
    ?assertEqual({num_val,1}, ?ENV_MODULE:lookup(n, C1Env)),
    {procedure, _, _, C2Env} = ?ENV_MODULE:lookup(c2, GlobalEnv),
    ?assertEqual({num_val,11}, ?ENV_MODULE:lookup(n, C2Env)),

    ?ENV_MODULE:stop_envs(GlobalEnv),
    ok.

fact_prog(Num) ->
    GlobalEnv = ?ENV_MODULE:init_global_env([{'+', {binf, fun({num_val, X}, {num_val, Y}) -> {num_val, X + Y} end}},
                                 {'*', {binf, fun({num_val, X}, {num_val, Y}) -> {num_val, X * Y} end}},
                                 {'<', {binf, fun({num_val, X}, {num_val, Y}) -> {bool_val, X < Y} end}},
                                 {'>', {binf, fun({num_val, X}, {num_val, Y}) -> {bool_val, X > Y} end}}]),
    Prog = [{define, fact_decl,
                {lambda, [n],
                    [{define, iter,
                        {lambda, [m,i],
                            [{'IF',
                                {apply,{var_exp,'>'},[{var_exp,i},{var_exp,n}]},
                                {var_exp,m},
                                {apply, {var_exp,iter},
                                    [{apply,{var_exp,'*'},[{var_exp,m},{var_exp,i}]},
                                     {apply,{var_exp,'+'},[{var_exp,i},{num_val,1}]}]}}]}},
                    {apply,{var_exp,iter},[{num_val,1},{num_val,1}]}]}},
            {apply,{var_exp,fact_decl},[{num_val,Num}]}],

    Res = envmodel:actual_value(Prog, GlobalEnv),
    ?ENV_MODULE:stop_envs(GlobalEnv),
    Res.

fact_test() ->
    ?assertEqual({num_val,120}, fact_prog(5)).
