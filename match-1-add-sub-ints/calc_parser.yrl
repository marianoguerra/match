Nonterminals 
predicate.

Terminals 
add_operator integer.

Rootsymbol predicate.

Left 300 add_operator.

predicate -> predicate add_operator predicate : {unwrap('$2'), '$1', '$3'}.

predicate -> integer : unwrap('$1').

Erlang code.

unwrap({_,_,V}) -> V.

