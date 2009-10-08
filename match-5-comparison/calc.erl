-module(calc).
-export([solve/1, get_tree/1]).

solve(String) ->
    matches(get_tree(String)).

get_tree(String) ->
    {ok, Tokens, _Endline} = calc_lexer:string(String),
    {ok, Tree} = calc_parser:parse(Tokens),
    Tree.

matches(A) when is_number(A) -> A;
matches({'+', A, B}) -> matches(A) + matches(B);
matches({'-', A, B}) -> matches(A) - matches(B);
matches({'*', A, B}) -> matches(A) * matches(B);
matches({'/', A, B}) -> matches(A) / matches(B);
matches({'%', A, B}) -> matches(A) rem matches(B);
matches({'<', A, B}) -> matches(A) < matches(B);
matches({'<=', A, B}) -> matches(A) =< matches(B);
matches({'==', A, B}) -> matches(A) =:= matches(B);
matches({'>=', A, B}) -> matches(A) >= matches(B);
matches({'>', A, B}) -> matches(A) > matches(B);
matches({'!=', A, B}) -> matches(A) =/= matches(B);
matches({'(', A}) -> matches(A);
matches(_) -> error.
