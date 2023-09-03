Nonterminals
exps exp apply vars
define_exp lambda_exp assign_exp cond_exp cond_subs if_exp and_exp or_exp let_exp let_subs let_star begin_exps
quote_exps quote_exp quote_in_exp quote_in_exps.

Terminals '(' ')'
num_val str_val bool_val var_exp
define lambda 'cond' else 'if' 'set!' 'and' 'or' 'let' 'begin' 'let*'
quote squote.

Rootsymbol exps.

exps -> exp : ['$1'].
exps -> exp exps : ['$1'|'$2'].

exp -> define_exp : '$1'.
exp -> lambda_exp : '$1'.
exp -> assign_exp : '$1'.
exp -> cond_exp : '$1'.
exp -> if_exp : '$1'.
exp -> and_exp : '$1'.
exp -> or_exp : '$1'.
exp -> let_exp : '$1'.
exp -> let_star : '$1'.
exp -> begin_exps : '$1'.
exp -> quote_exps : '$1'.
exp -> apply : '$1'.
exp -> num_val : tv('$1').
exp -> str_val : tv('$1').
exp -> bool_val : tv('$1').
exp -> var_exp : tv('$1').

vars -> '(' var_exp var_exp ')': [{value('$3'), value('$2')}].
vars -> var_exp: [value('$1')].
vars -> '(' var_exp var_exp ')' vars: [{value('$3'), value('$2')}|'$5'].
vars -> var_exp vars : [value('$1')|'$2'].

cond_subs -> '(' else exps ')' : '$3'.
cond_subs -> '(' exp exps ')' : {'IF', '$2', '$3', {bool_val, false}}.
cond_subs -> '(' exp exps ')' cond_subs : {'IF', '$2', '$3', '$5'}.

let_subs -> '(' var_exp exp ')': [{value('$2'), '$3'}].
let_subs -> '(' var_exp exp ')' let_subs: [{value('$2'), '$3'}|'$5'].

define_exp -> '(' define var_exp exps ')': {define, value('$3'), '$4'}.
define_exp -> '(' define '(' var_exp ')' exps ')': {define, value('$4'), {lambda, [], '$6'}}.
define_exp -> '(' define '(' var_exp vars ')' exps ')': {define, value('$4'), {lambda, '$5', '$7'}}.

lambda_exp -> '(' lambda '(' ')' exps ')': {lambda, [], '$5'}.
lambda_exp -> '(' lambda '(' vars ')' exps ')': {lambda, '$4', '$6'}.

assign_exp -> '(' 'set!' var_exp exp ')': {assign, value('$3'), '$4'}.

cond_exp -> '(' 'cond' cond_subs ')': '$3'.

if_exp -> '(' 'if' exp exp ')': {'IF', '$3', '$4', {bool_val, false}}.
if_exp -> '(' 'if' exp exp exp ')': {'IF', '$3', '$4', '$5'}.

and_exp -> '(' 'and' ')': {'AND', []}.
and_exp -> '(' 'and' exps ')': {'AND', '$3'}.

or_exp -> '(' 'or' ')': {'OR', []}.
or_exp -> '(' 'or' exps ')': {'OR', '$3'}.

let_exp -> '(' 'let' '(' let_subs ')' exps ')': {'LET', '$4', '$6'}.
let_exp -> '(' 'let' var_exp '(' let_subs ')' exps ')': {'LET', value('$3'), '$5', '$7'}.
let_star -> '(' 'let*' '(' let_subs ')' exps ')': {'LET*', '$4', '$6'}.

begin_exps -> '(' 'begin' exps ')': '$3'.

quote_in_exp -> num_val : tv('$1').
quote_in_exp -> str_val : tv('$1').
quote_in_exp -> bool_val : symbol('$1').
quote_in_exp -> var_exp : symbol('$1').
% define lambda 'cond' else 'if' 'set!' 'and' 'or' 'let' 'begin' 'let*'
quote_in_exp -> define : symbol('$1').
quote_in_exp -> lambda : symbol('$1').
quote_in_exp -> 'cond': symbol('$1').
quote_in_exp -> else : symbol('$1').
quote_in_exp -> 'if' : symbol ('$1').
quote_in_exp -> 'set!' : symbol ('$1').
quote_in_exp -> 'and' : symbol ('$1').
quote_in_exp -> 'or': symbol('$1').
quote_in_exp -> 'let' : symbol('$1').
quote_in_exp -> 'let*' : symbol('$1').
quote_in_exp -> quote : symbol('$1').

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

symbol({V, _}) ->
    {symbol, V};
symbol({_, _, V}) ->
    {symbol, V}.

% yecc:file("grm.yrl"). 