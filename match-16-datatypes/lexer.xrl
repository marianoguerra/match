Definitions.

Identifier	= [a-z_\?]
Number       	= [0-9]
AddOp     	= (\+|-)
MulOp     	= (\*|/|%)
CompOp    	= (<|<=|==|>=|>|!=)
BoolOp   	= (and|or)
White      	= [\s|\n]
Comment 	= #.*
Open    	= \(
Close   	= \)
OpenBlock    	= {
CloseBlock   	= }
OpenList	= \[
CloseList	= \]
Match		= =
Bool    	= (true|false)
Fn		= fn
Sep		= ,
End		= ;
UnaryOp		= (not|~)
% the string stuff taken from Reia
String	 	= "(\\\^.|\\.|[^\"])*"
Dot		= \.

Rules.

{AddOp}   		: {token, {add_op,	TokenLine, list_to_atom(TokenChars)}}.
{MulOp}   		: {token, {mul_op,	TokenLine, list_to_atom(TokenChars)}}.
{UnaryOp}   		: {token, {unary_op,	TokenLine, list_to_atom(TokenChars)}}.
{Number}+   		: {token, {integer,	TokenLine, list_to_integer(TokenChars)}}.
{Number}+\.{Number}+   	: {token, {float,	TokenLine, list_to_float(TokenChars)}}.
{Bool}   		: {token, {boolean,	TokenLine, list_to_atom(TokenChars)}}.
{BoolOp}   		: {token, {bool_op,	TokenLine, list_to_atom(TokenChars)}}.
{CompOp}   		: {token, {comp_op,	TokenLine, list_to_atom(TokenChars)}}.
{Open}			: {token, {open,	TokenLine, list_to_atom(TokenChars)}}.
{Close}			: {token, {close,	TokenLine, list_to_atom(TokenChars)}}.
{OpenBlock}		: {token, {open_block,	TokenLine, list_to_atom(TokenChars)}}.
{CloseBlock}		: {token, {close_block,	TokenLine, list_to_atom(TokenChars)}}.
{OpenList}		: {token, {open_list,	TokenLine, list_to_atom(TokenChars)}}.
{CloseList}		: {token, {close_list,	TokenLine, list_to_atom(TokenChars)}}.
{Fn}   			: {token, {fn,		TokenLine, list_to_atom(TokenChars)}}.
{Sep}   		: {token, {sep,		TokenLine, list_to_atom(TokenChars)}}.
{Dot}   		: {token, {dot,		TokenLine, list_to_atom(TokenChars)}}.
{End}   		: {token, {endl,	TokenLine, list_to_atom(TokenChars)}}.
{Match}			: {token, {match,	TokenLine, list_to_atom(TokenChars)}}.
{Identifier}+   	: {token, {var,		TokenLine, list_to_atom(TokenChars)}}.
:{Identifier}+   	: {token, {atom,	TokenLine, TokenChars}}.
&			: {token, {and_op,	TokenLine, list_to_atom(TokenChars)}}.
!			: {token, {or_op,	TokenLine, list_to_atom(TokenChars)}}.
\^			: {token, {xor_op,	TokenLine, list_to_atom(TokenChars)}}.
{White}+  		: skip_token.
{Comment}		: skip_token.
{String} 		: build_string(string, TokenChars, TokenLine, TokenLen).


Erlang code.

build_string(Type, Chars, Line, Len) ->
  String = unescape_string(lists:sublist(Chars, 2, Len - 2)),
    {token, {Type, Line, String}}.

unescape_string(String) -> unescape_string(String, []).
 
unescape_string([], Output) ->
  lists:reverse(Output);
unescape_string([$\\, Escaped | Rest], Output) ->
  Char = case Escaped of
    $\\ -> $\\;
    $/ -> $/;
    $\" -> $\";
    $\' -> $\';
    $b -> $\b;
    $d -> $\d;
    $e -> $\e;
    $f -> $\f;
    $n -> $\n;
    $r -> $\r;
    $s -> $\s;
    $t -> $\t;
    $v -> $\v;
    _ -> throw({error, {"unrecognized escape sequence: ", [$\\, Escaped]}})
  end,
  unescape_string(Rest, [Char|Output]);
unescape_string([Char|Rest], Output) ->
  unescape_string(Rest, [Char|Output]).
