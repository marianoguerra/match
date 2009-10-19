Nonterminals 
predicate literal expression function_def argument_def arguments block.

Terminals 
add_operator mul_operator match var comparation logic open close fn sep open_block close_block integer float boolean.

Rootsymbol predicate.

Nonassoc 100 match.
Left 200 open.
Left 300 logic.
Left 400 comparation.
Left 500 add_operator.
Left 600 mul_operator.

predicate -> expression: '$1'.

expression -> literal: '$1'.
expression -> open expression close : {line('$1'), unwrap('$1'), '$2'}.
expression -> expression add_operator expression : {line('$2'), unwrap('$2'), '$1', '$3'}.
expression -> expression mul_operator expression : {line('$2'), unwrap('$2'), '$1', '$3'}.
expression -> expression comparation expression : {line('$2'), unwrap('$2'), '$1', '$3'}.
expression -> expression logic expression : {line('$2'), unwrap('$2'), '$1', '$3'}.
expression -> expression match expression : {line('$2'), unwrap('$2'), unwrap('$1'), '$3'}.
expression -> function_def: '$1'.

function_def 	-> fn argument_def block: {line('$1'), unwrap('$1'), '$2', '$3'}.
argument_def	-> open arguments close : {line('$1'), unwrap('$1'), lists:flatten('$2')}.
argument_def	-> open close : {line('$1'), '(', []}.

arguments	-> var: ['$1'].
arguments	-> var sep arguments : ['$1', '$3'].

block		-> expression :  {line('$1'), '{', '$1'}.
block		-> open_block expression close_block : {line('$1'), unwrap('$1'), '$2'}.

literal -> integer : {integer, line('$1'), unwrap('$1')}.
literal -> float : {float, line('$1'), unwrap('$1')}.
literal -> boolean: {atom, line('$1'), unwrap('$1')}.
literal -> var: {var, line('$1'), unwrap('$1')}.

Erlang code.

unwrap({_,V}) -> V;
unwrap({_,_,V}) -> V.

line({Line, _}) -> Line;
line({_, Line, _}) -> Line;
line({Line, _, _, _}) -> Line.

