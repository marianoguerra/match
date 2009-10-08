Definitions.

D   = [0-9]
AOP   = (\+|-)
MOP   = (\*|/|%)
WS  = ([\000-\s]|#.*)

Rules.

{AOP}   : {token,{add_operator,TokenLine,list_to_atom(TokenChars)}}.
{MOP}   : {token,{mul_operator,TokenLine,list_to_atom(TokenChars)}}.
{D}+   : {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
{D}+\.{D}+   : {token,{float,TokenLine,list_to_float(TokenChars)}}.
{WS}+  : skip_token.

Erlang code.

