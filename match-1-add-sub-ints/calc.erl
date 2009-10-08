-module(calc).
-export([solve/1]).

solve(String) ->
    {ok, Tokens, _Endline} = calc_lexer:string(String),
    {ok, Tree} = calc_parser:parse(Tokens),
    matches(Tree).

matches(A) when is_integer(A) -> A;
matches({'+', A, B}) -> matches(A) + matches(B);
matches({'-', A, B}) -> matches(A) - matches(B);
matches(_) -> error.
