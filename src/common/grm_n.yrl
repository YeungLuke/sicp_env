Nonterminals
exps exp apply vars
special_exp
quote_exps quote_exp quote_in_exp quote_in_exps.

Terminals '(' ')'
num_val str_val bool_val var_exp
special
quote squote.

Rootsymbol exps.

exps -> exp : ['$1'].
exps -> exp exps : ['$1'|'$2'].

exp -> special_exp : '$1'.
exp -> quote_exps : '$1'.
exp -> apply : '$1'.
exp -> num_val : tv('$1').
exp -> str_val : tv('$1').
exp -> bool_val : tv('$1').
exp -> var_exp : tv('$1').


special_exp -> '(' special exps ')' : special_exp(special('$2'), '$3').
% special_exp -> '(' special '(' vars ')' exps ')': special_exp(special('$2'), '$4', '$6').

special_exp -> '(' special '(' ')' exps ')': {special('$2'), [], '$5'}.

% cond_subs -> '(' else exps ')' : '$3'.
% cond_subs -> '(' exp exps ')' : {'IF', '$2', '$3', {bool_val, false}}.
% cond_subs -> '(' exp exps ')' cond_subs : {'IF', '$2', '$3', '$5'}.

% let_subs -> '(' var_exp exp ')': [{value('$2'), '$3'}].
% let_subs -> '(' var_exp exp ')' let_subs: [{value('$2'), '$3'}|'$5'].

% define_exp -> '(' define var_exp exps ')': {define, value('$3'), '$4'}.
% define_exp -> '(' define '(' var_exp ')' exps ')': {define, value('$4'), {lambda, [], '$6'}}.
% define_exp -> '(' define '(' var_exp vars ')' exps ')': {define, value('$4'), {lambda, '$5', '$7'}}.

% lambda_exp -> '(' lambda '(' ')' exps ')': {lambda, [], '$5'}.
% lambda_exp -> '(' lambda '(' vars ')' exps ')': {lambda, '$4', '$6'}.

% assign_exp -> '(' 'set!' var_exp exp ')': {assign, value('$3'), '$4'}.

% cond_exp -> '(' 'cond' cond_subs ')': '$3'.

% if_exp -> '(' 'if' exp exp ')': {'IF', '$3', '$4', {bool_val, false}}.
% if_exp -> '(' 'if' exp exp exp ')': {'IF', '$3', '$4', '$5'}.

% and_exp -> '(' 'and' ')': {'AND', []}.
% and_exp -> '(' 'and' exps ')': {'AND', '$3'}.

% or_exp -> '(' 'or' ')': {'OR', []}.
% or_exp -> '(' 'or' exps ')': {'OR', '$3'}.

% let_exp -> '(' 'let' '(' let_subs ')' exps ')': {'LET', '$4', '$6'}.
% let_exp -> '(' 'let' var_exp '(' let_subs ')' exps ')': {'LET', value('$3'), '$5', '$7'}.
% let_star -> '(' 'let*' '(' let_subs ')' exps ')': {'LET*', '$4', '$6'}.

% begin_exps -> '(' 'begin' exps ')': '$3'.

quote_in_exp -> num_val : tv('$1').
quote_in_exp -> str_val : tv('$1').
quote_in_exp -> bool_val : symbol('$1').
quote_in_exp -> var_exp : symbol('$1').
quote_in_exp -> special : symbol('$1').

quote_in_exps -> quote_exp : ['$1'].
quote_in_exps -> quote_exp quote_in_exps : ['$1'|'$2'].
quote_exp -> quote_in_exp : '$1'.
% 'exp to (quote exp)
quote_exp -> squote quote_exp : global_frame:make_list([{symbol, quote}, '$2']).
quote_exp -> '(' ')' : global_frame:make_list([]).
quote_exp -> '(' quote_in_exps ')' : global_frame:make_list('$2').

quote_exps ->  '(' quote quote_exp ')' : {quote, '$3'}.
quote_exps ->  squote quote_exp : {quote, '$2'}.

apply -> '(' exp ')': {apply, '$2', []}.
apply -> '(' exp exps ')': {apply, '$2', '$3'}.

Erlang code.

tv({T, _, V}) ->
    {T, V}.

value({_, _, V}) ->
    V.

special({special, _, 'set!'}) ->
    assign;
special({special, _, V}) ->
    V.

special_exp(define, [{var_exp, V}|Body]) ->
    {define, V, Body};
special_exp(assign, [{var_exp, V}|Body]) ->
    {assign, V, Body};
special_exp('if', [H1, H2]) ->
    {'IF', H1, H2, {bool_val, false}};
special_exp('if', [H1, H2, H3]) ->
    {'IF', H1, H2, H3}.

special_exp(define, [H|Vars], Body) ->
    {define, {var_exp, H}, {lambda, Vars, Body}};
special_exp(lambda, Vars, Body) ->
    {lambda, Vars, Body}.

symbol({V, _}) ->
    {symbol, V};
symbol({_, _, V}) ->
    {symbol, V}.

% yecc:file("grm.yrl"). 