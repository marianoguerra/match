Nonterminals 
predicate.

Terminals 
add_operator mul_operator open close integer float.

Rootsymbol predicate.

Left 100 open.
Left 300 add_operator.
Left 400 mul_operator.

predicate -> open predicate close : {unwrap('$1'), '$2'}.
predicate -> predicate add_operator predicate : {unwrap('$2'), '$1', '$3'}.
predicate -> predicate mul_operator predicate : {unwrap('$2'), '$1', '$3'}.

predicate -> integer : unwrap('$1').
predicate -> float : unwrap('$1').

Erlang code.

unwrap({_,_,V}) -> V.

