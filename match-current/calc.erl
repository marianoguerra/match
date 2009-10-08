-module(calc).
-export([solve/1, get_tree/1, mod/2]).

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
matches({'%', A, B}) -> mod(matches(A), matches(B));
matches(_) -> error.

mod(X, Y) when is_float(X) or is_float(Y) -> 0.0;
mod(X, Y) when X > 0 -> X rem Y;
mod(X, Y) when X < 0 -> Y + X rem Y;
mod(0, _Y) -> 0.

