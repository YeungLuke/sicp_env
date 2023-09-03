Definitions.

SPECIAL = define|lambda|set!|cond|else|if|and|or|let\*|let|begin
D       = [0-9]
V       = [^'\s\t\n\r()]
COMMENT = ;.*

Rules.

' : {token, {squote, TokenLine}}.
[+-]?{D}+ : {token, {num_val, TokenLine, list_to_integer(TokenChars)}}.
[+-]?{D}+\.{D}+((E|e)(\+|-)?{D}+)? : {token, {num_val, TokenLine, list_to_float(TokenChars)}}.
[()]|{SPECIAL} : {token, {list_to_atom(TokenChars), TokenLine}}.
".+" : {token, {str_val, TokenLine, strip(TokenChars, TokenLen)}}. 
true|false : {token, {bool_val, TokenLine, list_to_atom(TokenChars)}}.
{V}+ : {token, {var_exp, TokenLine, list_to_atom(TokenChars)}}.
[\s\t\n\r]+ : skip_token.
{COMMENT} : skip_token.

Erlang code.

strip(TokenChars, TokenLen) ->
    lists:sublist(TokenChars, 2, TokenLen - 2).
% leex:file("lex.xrl").
