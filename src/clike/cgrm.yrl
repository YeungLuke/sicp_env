Nonterminals
exps exp vars else_exp else_if_exp args 
exp_and exp_or
exp_assign
exp_equal exp_add exp_mul
exp_define exp_if exp_fun exp_apply exp_block.

Terminals
equal_exp add_exp mul_exp
end_exp
var func 'if' else 'and' 'or'
'(' ')' '{' '}' '=' ',' '=!'
num_val str_val bool_val var_exp.

Rootsymbol exps.

Left 150 '=!'.
Left 150 'or'.
Left 160 'and'.
Left 200 '='.
Left 200 equal_exp.
Left 400 add_exp.
Left 500 mul_exp.
Nonassoc 1000 '(' ')'.
Nonassoc 1100 '{' '}'.

exps -> exp end_exp : ['$1'].
exps -> exp_block : ['$1'].
exps -> exp_fun : ['$1'].
exps -> exp_if : ['$1'].
exps -> exp end_exp exps : ['$1'|'$3'].
exps -> exp_block exps : ['$1'|'$2'].
exps -> exp_fun exps : ['$1'|'$2'].
exps -> exp_if exps : ['$1'|'$2'].

exp -> exp_and : '$1'.
exp -> exp_or : '$1'.
exp -> exp_assign : '$1'.
exp -> exp_equal : '$1'.
exp -> exp_add : '$1'.
exp -> exp_mul : '$1'.
exp -> exp_define : '$1'.
exp -> exp_if: '$1'.
exp -> exp_fun : '$1'.
exp -> exp_apply : '$1'.
exp -> exp_block: '$1'.
exp -> '(' exp ')' : '$2'.
exp -> num_val : tv('$1').
exp -> str_val : tv('$1').
exp -> bool_val : tv('$1').
exp -> var_exp : tv('$1').

vars -> '$empty' : [].
vars -> '(' var_exp var_exp ')' : [{value('$3'), value('$2')}].
vars -> var_exp: [value('$1')].
vars -> '(' var_exp var_exp ')' ',' vars : [{value('$3'), value('$2')}|'$6'].
vars -> var_exp ',' vars : [value('$1')|'$3'].

args -> '$empty' : [].
args -> exp : ['$1'].
args -> exp ',' args : ['$1'|'$3'].

exp_and -> exp 'and' exp : {'AND', ['$1', '$3']}.
exp_or -> exp 'or' exp : {'OR', ['$1', '$3']}.

exp_assign -> var_exp '=!' exp : {assign, value('$1'), '$3'}.

exp_equal -> exp '=' exp : {apply, var_exp('$2'), ['$1', '$3']}.
exp_equal -> exp equal_exp exp : {apply, var_exp('$2'), ['$1', '$3']}.
exp_add -> exp add_exp exp : {apply, var_exp('$2'), ['$1', '$3']}.
exp_mul -> exp mul_exp exp : {apply, var_exp('$2'), ['$1', '$3']}.

exp_define -> var var_exp '=' exp : {define, value('$2'), '$4'}.

else_exp -> else '{' exps '}' : '$3'.
else_if_exp -> else 'if' '(' exp ')' '{' exps '}' : {'IF', '$4', '$7', bool_false()}.
else_if_exp -> else 'if' '(' exp ')' '{' exps '}' else_if_exp : {'IF', '$4', '$7', '$9'}.
else_if_exp -> else 'if' '(' exp ')' '{' exps '}' else_exp : {'IF', '$4', '$7', '$9'}.
exp_if -> 'if' '(' exp ')' '{' exps '}' : {'IF', '$3', '$6', bool_false()}.
exp_if -> 'if' '(' exp ')' '{' exps '}' else_if_exp : {'IF', '$3', '$6', '$8'}.
exp_if -> 'if' '(' exp ')' '{' exps '}' else_exp : {'IF', '$3', '$6', '$8'}.

exp_fun -> func '(' vars ')' '{' exps '}' : {lambda, '$3', '$6'}.
exp_fun -> func var_exp '(' vars ')' '{' exps '}' : {define, value('$2'), {lambda, '$4', '$7'}}.

exp_apply -> exp '(' args ')' : {apply, '$1', '$3'}.

exp_block -> '{' exps '}' : {'LET', [], '$2'}.


Erlang code.
tv({T, _, V}) -> {T, V}.
value({_, _, V}) -> V.
var_exp({V, _}) -> {var_exp, V};
var_exp({_,_,V}) -> {var_exp, V}.
bool_false() -> {bool_val, false}.