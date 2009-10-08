Nonterminals 
predicate.

Terminals 
add_operator mul_operator comparation logic open close integer float boolean.

Rootsymbol predicate.

Left 100 open.
Left 200 logic.
Left 300 comparation.
Left 400 add_operator.
Left 500 mul_operator.

predicate -> open predicate close : {unwrap('$1'), '$2'}.
predicate -> predicate add_operator predicate : {unwrap('$2'), '$1', '$3'}.
predicate -> predicate mul_operator predicate : {unwrap('$2'), '$1', '$3'}.
predicate -> predicate comparation predicate : {unwrap('$2'), '$1', '$3'}.
predicate -> predicate logic predicate : {unwrap('$2'), '$1', '$3'}.

predicate -> integer : unwrap('$1').
predicate -> float : unwrap('$1').
predicate -> boolean: unwrap('$1').

Erlang code.

unwrap({_,_,V}) -> V.

