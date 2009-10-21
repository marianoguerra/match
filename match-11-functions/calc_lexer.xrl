Definitions.

VAR	= [a-z_]
D       = [0-9]
AOP     = (\+|-)
MOP     = (\*|/|%)
COMP    = (<|<=|==|>=|>|!=)
LOGIC   = (and|or)
WS      = [\s|\n]
COMMENT = #.*
OPEN    = \(
CLOSE   = \)
OPEN_BLOCK    = \{
CLOSE_BLOCK   = \}
MATCH	= =
BOOL    = (true|false)
FN	= fn
SEP	= ,
END	= ;

Rules.

{AOP}   	: {token,{add_op,TokenLine,list_to_atom(TokenChars)}}.
{MOP}   	: {token,{mul_op,TokenLine,list_to_atom(TokenChars)}}.
{D}+   		: {token,{integer,TokenLine,list_to_integer(TokenChars)}}.
{D}+\.{D}+   	: {token,{float,TokenLine,list_to_float(TokenChars)}}.
{BOOL}   	: {token,{boolean,TokenLine,list_to_atom(TokenChars)}}.
{LOGIC}   	: {token,{bool_op,TokenLine,list_to_atom(TokenChars)}}.
{COMP}   	: {token,{comp_op,TokenLine,list_to_atom(TokenChars)}}.
{OPEN}		: {token,{open,TokenLine,list_to_atom(TokenChars)}}.
{CLOSE}		: {token,{close,TokenLine,list_to_atom(TokenChars)}}.
{OPEN_BLOCK}		: {token,{open_block,TokenLine,list_to_atom(TokenChars)}}.
{CLOSE_BLOCK}		: {token,{close_block,TokenLine,list_to_atom(TokenChars)}}.
{FN}   		: {token,{fn,TokenLine,list_to_atom(TokenChars)}}.
{SEP}   	: {token,{sep,TokenLine,list_to_atom(TokenChars)}}.
{END}   	: {token,{endl,TokenLine,list_to_atom(TokenChars)}}.
{MATCH}		: {token,{match,TokenLine,list_to_atom(TokenChars)}}.
{VAR}+   	: {token,{var,TokenLine,list_to_atom(TokenChars)}}.
\.{VAR}+   	: {token,{atom,TokenLine,TokenChars}}.
{WS}+  		: skip_token.
{COMMENT}	: skip_token.


Erlang code.

