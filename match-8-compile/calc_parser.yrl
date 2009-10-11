Nonterminals 
predicate.

Terminals 
add_operator mul_operator match var comparation logic open close integer float boolean.

Rootsymbol predicate.

Nonassoc 100 match.
Left 200 open.
Left 300 logic.
Left 400 comparation.
Left 500 add_operator.
Left 600 mul_operator.

predicate -> open predicate close : {line('$1'), unwrap('$1'), '$2'}.
predicate -> predicate add_operator predicate : {line('$2'), unwrap('$2'), '$1', '$3'}.
predicate -> predicate mul_operator predicate : {line('$2'), unwrap('$2'), '$1', '$3'}.
predicate -> predicate comparation predicate : {line('$2'), unwrap('$2'), '$1', '$3'}.
predicate -> predicate logic predicate : {line('$2'), unwrap('$2'), '$1', '$3'}.
predicate -> var match predicate : {line('$2'), unwrap('$2'), unwrap('$1'), '$3'}.

predicate -> integer : {line('$1'), unwrap('$1')}.
predicate -> float : {line('$1'), unwrap('$1')}.
predicate -> boolean: {line('$1'), unwrap('$1')}.
predicate -> var: {line('$1'), unwrap('$1')}.

Erlang code.

unwrap({_,_,V}) -> V.
line({_, Line, _}) -> Line.

