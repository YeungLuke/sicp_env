-module(make_exp).
-export([make_exp/1]).

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

assign(Var, Body) ->
    {assign, Var, Body}.

if_exp(Predicate, Consequent, Alternative) ->
    {'IF', Predicate, Consequent, Alternative}.

if_exp(Predicate, Consequent) ->
    {'IF', Predicate, Consequent, const_exp(false)}.

make_and(Exps) ->
    {'AND', Exps}.

make_or(Exps) ->
    {'OR', Exps}.

make_let(Var, Bindings, Body) ->
    {'LET', Var, Bindings, Body}.

make_let(VarExps, Body) ->
    {'LET', VarExps, Body}.

make_let_star(VarExps, Body) ->
    {'LET*', VarExps, Body}.

make_while(Predicate, Body) ->
    {while, Predicate, Body}.
% make end

% make exp begin
make_sequence_exp(Input) ->
    lists:map(fun(Sub) -> make_exp(Sub) end, Input).

is_number_exp(Input) ->
    try
        {true, binary_to_integer(Input)}
    catch _:_ ->
        try
            {true, binary_to_float(Input)}
        catch _:_ ->
            false
        end
    end.

make_var([Var, LazyMode]) ->
    {binary_to_atom(LazyMode, utf8), binary_to_atom(Var, utf8)};
make_var(Var) ->
    binary_to_atom(Var, utf8).

make_define_to_define_lambda(Name, Vars, Body) ->
    define(make_var(Name), lambda(lists:map(fun(Sub) -> make_var(Sub) end, Vars), make_sequence_exp(Body))).

is_cond_else_clause([<<"else">>|_]) ->
    true;
is_cond_else_clause(_) ->
    false.

cond_predicate([H|_]) ->
    H.

cond_actions([_|T]) ->
    T.

make_cond_to_if([]) ->
    false;
make_cond_to_if([H|T]=Clauses) ->
    case {is_cond_else_clause(H), T} of
        {true, []} ->
            make_sequence_exp(cond_actions(H));
        {true, _} ->
            error("make cond to if: else clause isn't last", [Clauses]);
        {false, _} ->
            if_exp(make_exp(cond_predicate(H)),
                   make_sequence_exp(cond_actions(H)),
                   make_cond_to_if(T))
    end.

make_var_exps(VarExps) ->
    lists:map(fun([Var, Exp]) -> {make_var(Var), make_exp(Exp)} end, VarExps).

make_element(<<"true">>) ->
    const_exp(true);
make_element(<<"false">>) ->
    const_exp(false);
make_element(<<"+">>) ->
    var_exp('+');
make_element(<<"-">>) ->
    var_exp('-');
make_element(<<H:8/integer, BinStr/binary>>) when H =:= $" ->
    Str = binary_to_list(BinStr),
    const_exp(lists:sublist(Str, length(Str) - 1));
make_element(Input) ->
    case is_number_exp(Input) of
        {true, Num} ->
            const_exp(Num);
        _ ->
            var_exp(make_var(Input))
    end.

make_exp(Input) when is_binary(Input) ->
    make_element(Input);
make_exp([<<"define">>, [Name | Vars] | Body]) ->
    make_define_to_define_lambda(Name, Vars, Body);
make_exp([<<"define">>, Var | Body]) ->
    define(make_var(Var), make_sequence_exp(Body));
make_exp([<<"lambda">>, Vars | Body]) ->
    lambda(lists:map(fun(Sub) -> make_var(Sub) end, Vars), make_sequence_exp(Body));
make_exp([<<"set!">>, Var, Body]) ->
    assign(make_var(Var), make_exp(Body));
make_exp([<<"cond">> | Body]) ->
    make_cond_to_if(Body);
make_exp([<<"if">>, Predicate, Consequent]) ->
    if_exp(make_exp(Predicate), make_exp(Consequent));
make_exp([<<"if">>, Predicate, Consequent, Alternative]) ->
    if_exp(make_exp(Predicate), make_exp(Consequent), make_exp(Alternative));
make_exp([<<"and">>| Conditions]) ->
    make_and(make_sequence_exp(Conditions));
make_exp([<<"or">> | Conditions]) ->
    make_or(make_sequence_exp(Conditions));
make_exp([<<"let">>, VarExps | Body]) when is_list(VarExps) ->
    make_let(make_var_exps(VarExps), make_sequence_exp(Body));
make_exp([<<"let">>, Var, Bindings | Body]) ->
    make_let(make_var(Var), make_var_exps(Bindings), make_sequence_exp(Body));
make_exp([<<"let*">>, VarExps | Body]) ->
    make_let_star(make_var_exps(VarExps), make_sequence_exp(Body));
make_exp([<<"while">>, Predicate | Body]) ->
    make_while(make_exp(Predicate), make_sequence_exp(Body));
make_exp([<<"begin">> | Sequence]) ->
    make_sequence_exp(Sequence);
% apply
make_exp([H | T]) ->
    app(make_exp(H), make_sequence_exp(T));
make_exp(Input) ->
    error("make exp error", [Input]).
% make exp end