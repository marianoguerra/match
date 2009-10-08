Definitions.

D       = [0-9]
AOP     = (\+|-)
MOP     = (\*|/|%)
COMP     = (<|<=|==|>=|>|!=)
WS      = ([\000-\s]|#.*)
OPEN    = \(
CLOSE   = \)
BOOL    = (true|false)

Rules.

{AOP}   : {token,{add_operator,TokenLine,list_to_atom(TokenChars)}}.
{MOP}   : {token,{mul_operator,TokenLine,list_to_atom(TokenChars)}}.
{D}+   : {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
{D}+\.{D}+   : {token,{float,TokenLine,list_to_float(TokenChars)}}.
{BOOL}   : {token,{boolean,TokenLine,list_to_atom(TokenChars)}}.
{COMP}   : {token,{comparation,TokenLine,list_to_atom(TokenChars)}}.
{OPEN}	: {token,{open,TokenLine,list_to_atom(TokenChars)}}.
{CLOSE}	: {token,{close,TokenLine,list_to_atom(TokenChars)}}.
{WS}+  : skip_token.


Erlang code.

