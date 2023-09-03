-module(envmodel).

-export([actual_value/2]).

-include("env.hrl").

-define(LAZY, true).

lookup_variable_value({var_exp, Var}, Env) ->
    ?ENV_MODULE:lookup(Var, Env).

set_variable_value(Var, Value, Env) ->
    ?ENV_MODULE:set(Var, Value, Env).

define_variable(Var, Value, Env) ->
    ?ENV_MODULE:define(Var, Value, Env).

% make
lambda(Parameters, Body) when is_list(Body) ->
    {lambda, Parameters, Body};
lambda(Parameters, Body) ->
    {lambda, Parameters, [Body]}.

var_exp(Var) ->
    {var_exp, Var}.

const_exp(Num) when is_number(Num) ->
    {num_val, Num};
const_exp(Bool) when is_boolean(Bool) ->
    {bool_val, Bool};
const_exp(Str) when is_list(Str) ->
    {str_val, Str}.

app(Exp, Vars) ->
    {apply, Exp, Vars}.

define(Var, Body) ->
    {define, Var, Body}.

if_exp(Predicate, Consequent, Alternative) ->
    {'IF', Predicate, Consequent, Alternative}.

make_let(Var, Bindings, Body) ->
    {'LET', Var, Bindings, Body}.

make_let(VarExps, Body) ->
    {'LET', VarExps, Body}.

make_let_star(VarExps, Body) ->
    {'LET*', VarExps, Body}.
% make end

lambda_parameters({lambda, Parameters, _}) ->
    Parameters.
lambda_body({lambda, _, Body}) ->
    Body.

make_procedure(Parameters, Body, Env) ->
    {procedure, Parameters, Body, Env}.

is_primitive_procedure({binf, _}) ->
    true;
is_primitive_procedure({mul_args, _}) ->
    true;
is_primitive_procedure(_) ->
    false.

apply_primitive_procedure({binf, Fun}, Arguments) ->
    apply(Fun, Arguments);
apply_primitive_procedure({mul_args, Fun}, Arguments) ->
    apply(Fun, [Arguments]).

is_compound_procedure({procedure, _, _, _}) ->
    true;
is_compound_procedure(_) ->
    false.

operator({apply, Operator, _}) -> Operator.
operands({apply, _, Operands}) -> Operands.

-ifndef(LAZY).

list_of_value([], _) ->
    [];
list_of_value([H|T], Env) ->
    [eval(H, Env) | list_of_value(T, Env)].

apply_compound_procedure({procedure, Parameters, Body, Env}, Arguments) ->
    % io:format("apply_compound_procedure/2 ~p ~p~n~p~n", [Env, Env, Body]),
    NewEnv = ?ENV_MODULE:extend_env(Parameters, Arguments, Env),
    Result = eval_sequence(Body, NewEnv),
    ?ENV_MODULE:mark_remove_env(NewEnv, Result, Arguments),
    Result.

applies(Procedure, Arguments) ->
    case is_primitive_procedure(Procedure) of
        true ->
            apply_primitive_procedure(Procedure, Arguments);
        _ ->
            case is_compound_procedure(Procedure) of
                true ->
                    apply_compound_procedure(Procedure, Arguments);
                _ ->
                    error("apply unkonw procedure type", [Procedure])
            end
    end.


eval_apply(Exp, Env) ->
    applies(eval(operator(Exp), Env), list_of_value(operands(Exp), Env)).

actual_value(Exp, Env) ->
    eval(Exp, Env).

-else.

% lazy eval
eval_apply(Exp, Env) ->
    applies(actual_value(operator(Exp), Env), operands(Exp), Env).

actual_value(Exp, Env) ->
    % io:format("actual_value env: ~p, exp: ~p, pid: ~p~n", [Env, Exp, self()]),
    force_it(eval(Exp, Env)).

force_it({thunk_memo, ThunkPid, Env}) ->
    ?ENV_MODULE:force_thunk(ThunkPid, Env);
force_it({thunk, Exp, Env}) ->
    actual_value(Exp, Env);
force_it(Obj) ->
    Obj.

delay_it(Exp, Env) ->
    {thunk, Exp, Env}.

delay_memory(Exp, Env) ->
    % {thunk_memo, spawn(fun() -> thunk_loop(Exp, Env) end), Env}.
    {thunk_memo, ?ENV_MODULE:new_thunk(Exp, Env), Env}.

list_of_arg_values([], _) ->
    [];
list_of_arg_values([H|T], Env) ->
    [actual_value(H, Env) | list_of_arg_values(T, Env)].

list_of_delayed_args(_, [], _) ->
    [];
list_of_delayed_args([{'lazy-memo', _P}|R], [H|T], Env) ->
    [delay_memory(H, Env) | list_of_delayed_args(R, T, Env)];
list_of_delayed_args([{lazy, _P}|R], [H|T], Env) ->
    [delay_it(H, Env) | list_of_delayed_args(R, T, Env)];
list_of_delayed_args([_P|R], [H|T], Env) ->
    [actual_value(H, Env) | list_of_delayed_args(R, T, Env)].

apply_compound_procedure({procedure, Parameters, Body, PEnv}, Arguments, Env) ->
    ActualParameters = lists:map(fun({_LazyMode, P}) -> P;
                                     (P) -> P end, Parameters),
    % io:format("apply_compound_procedure/3 parent ~p args ~p~n~p~n", [PEnv, Env, Body]),
    ArgValues = list_of_delayed_args(Parameters, Arguments, Env),
    NewEnv = ?ENV_MODULE:extend_env(ActualParameters, ArgValues, PEnv),
    Result = eval_sequence(Body, NewEnv),
    % io:format("apply_compound_procedure/3 mark_remove_env parent ~p args ~p new ~p~n", [PEnv, Env, NewEnv]),
    ?ENV_MODULE:mark_remove_env(NewEnv, Result, ArgValues),
    Result.

applies(Procedure, Arguments, Env) ->
    case is_primitive_procedure(Procedure) of
        true ->
            apply_primitive_procedure(Procedure, list_of_arg_values(Arguments, Env));
        _ ->
            case is_compound_procedure(Procedure) of
                true ->
                    apply_compound_procedure(Procedure, Arguments, Env);
                _ ->
                    error("apply unkonw procedure type", [Procedure])
            end
    end.
%  lazy eval end
-endif.

eval_quote({quote, Body}, _Env) ->
    Body.

eval_assignment({assign, Var, Body}, Env) ->
    set_variable_value(Var, eval(Body, Env), Env),
    ok.

eval_definition({define, Var, Body}, Env) ->
    define_variable(Var, eval (Body, Env), Env),
    ok.

is_true({num_val, N}) ->
    N =/= 0;
is_true({bool_val, false}) ->
    false;
is_true(_) ->
    true.

eval_if({'IF', Predicate, Consequent, Alternative}, Env) ->
    case is_true(actual_value(Predicate, Env)) of
        true ->
            eval(Consequent, Env);
        _ ->
            eval(Alternative, Env)
    end.

eval_sequence([Last], Env) ->
    eval(Last, Env);
eval_sequence([H|T], Env) ->
    eval(H, Env),
    eval_sequence(T, Env).

and_to_if([]) ->
    const_exp(true);
and_to_if([H|T]) ->
    if_exp(H, and_to_if(T), const_exp(false)).

or_to_if([]) ->
    const_exp(false);
or_to_if([H|T]) ->
    if_exp(H, const_exp(true), or_to_if(T)).

let_to_combination({'LET', VarExps, Body}) ->
    {Vars, Exps} = lists:unzip(VarExps),
    app(lambda(Vars, Body), Exps);
let_to_combination({'LET', Var, Bindings, Body}) ->
    {Vars, Exps} = lists:unzip(Bindings),
    [define(Var, lambda(Vars, Body)), app(var_exp(Var), Exps)].

let_star_to_nested_lets({'LET*', [Last], Body}) ->
    make_let([Last], Body);
let_star_to_nested_lets({'LET*', [H|T], Body})  ->
    make_let([H], let_star_to_nested_lets(make_let_star(T, Body))).

while_to_combination({while, Predicate, Body}) ->
    make_let('__while__',
             [],
             if_exp(Predicate,
                    [Body, app(var_exp('__while__'), [])],
                     const_exp(true))).

% self-evaluation?
eval({num_val, _} = Exp, _Env) ->
    Exp;
eval({bool_val, _} = Exp, _Env) ->
    Exp;
eval({str_val, _}= Exp, _Env) ->
    Exp;
eval({pair_val, _} = Exp, _Env) ->
    Exp;
eval({symbol, _} = Exp, _Env) ->
    Exp;
% variable?
eval({var_exp, _} = Exp, Env) ->
    lookup_variable_value(Exp, Env);
% quoted
eval({quote, _} = Exp, Env) ->
    eval_quote(Exp, Env);
% assignment?
eval({assign, _, _} = Exp, Env) ->
    eval_assignment(Exp, Env);
% definition?
eval({define, _, _} = Exp, Env) ->
    eval_definition(Exp, Env);
% if?
eval({'IF', _, _, _} = Exp, Env) ->
    eval_if(Exp, Env);
% Lambda?
eval({lambda, _, _} = Exp, Env) ->
    make_procedure(lambda_parameters(Exp), lambda_body(Exp), Env);
% begin?
eval(Exps, Env) when is_list(Exps) ->
    eval_sequence(Exps, Env);
% cond? done in make exp
eval({'AND', Conditions}, Env) ->
    eval(and_to_if(Conditions), Env);
eval({'OR', Conditions}, Env) ->
    eval(or_to_if(Conditions), Env);
eval({'LET', _, _} = Exp, Env) ->
    eval(let_to_combination(Exp), Env);
eval({'LET', _, _, _} = Exp, Env) ->
    eval(let_to_combination(Exp), Env);
eval({'LET*', _, _}= Exp, Env) ->
    eval(let_star_to_nested_lets(Exp), Env);
eval({while, _, _} = Exp, Env) ->
    eval(while_to_combination(Exp), Env);
% application?
eval({apply, _, _}= Exp, Env) ->
    eval_apply(Exp, Env);
eval(Exp, _Env) ->
    error("eval unkonw expressoin type", [Exp]).
