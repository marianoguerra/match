-module(calc).
-export([solve/1, get_tree/1]).

solve(String) ->
    matches(get_tree(String)).

get_tree(String) ->
    {ok, Tokens, _Endline} = calc_lexer:string(String),
    {ok, Tree} = calc_parser:parse(Tokens),
    Tree.

matches(A) when is_number(A) -> A;
matches('true') -> true;
matches('false') -> false;
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

matches({'and', A, B}) ->
    First = matches(A),
    case as_boolean(First) of
        true -> matches(B);
        false -> First
    end;

matches({'or', A, B}) ->
    First = matches(A),
    case as_boolean(First) of
        true -> First;
        false -> matches(B)
    end;

matches({'(', A}) -> matches(A);
matches(_) -> error.

as_boolean(Val) when is_boolean(Val) -> Val;
as_boolean(Val) when is_number(Val) -> Val > 0.
