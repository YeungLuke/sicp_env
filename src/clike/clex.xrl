Definitions.

SPECIAL = var|\=!|\=|func|if|else|and|or
Variables = [a-zA-Z_][0-9a-zA-Z_!?]*
D = [0-9]
COMMENT = //.*


Rules.

{D}+ : {token, {num_val, TokenLine, list_to_integer(TokenChars)}}.
{D}+\.{D}+((E|e)(\+|\-)?{D}+)? : {token, {num_val, TokenLine, list_to_float(TokenChars)}}.
".+" : {token, {str_val, TokenLine, strip(TokenChars, TokenLen)}}. 
true|false : {token, {bool_val, TokenLine, list_to_atom(TokenChars)}}.

#|>|< : {token, {equal_exp, TokenLine, list_to_atom(TokenChars)}}.
[+-] : {token, {add_exp, TokenLine, list_to_atom(TokenChars)}}.
[*/\%] : {token, {mul_exp, TokenLine, list_to_atom(TokenChars)}}.
[(){},]|{SPECIAL} : {token, {list_to_atom(TokenChars), TokenLine}}.

{Variables} : {token, {var_exp, TokenLine, list_to_atom(TokenChars)}}.
[;] : {token, {end_exp, TokenLine}}.
{COMMENT} : skip_token.
[\s\t\n\r]+ : skip_token.


Erlang code.

strip(TokenChars, TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 2).
% leex:file("lex.xrl").
