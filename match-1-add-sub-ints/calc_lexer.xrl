Definitions.

D   = [0-9]
AOP   = (\+|-)
WS  = ([\000-\s]|#.*)

Rules.

{AOP}   : {token,{add_operator,TokenLine,list_to_atom(TokenChars)}}.
{D}+   : {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
{WS}+  : skip_token.

Erlang code.

