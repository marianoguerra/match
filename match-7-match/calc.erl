-module(calc).
-export([solve/1, solve/2, get_tree/1]).

solve(String) ->
    solve(String, dict:new()).

solve(String, Context) ->
    matches(get_tree(String), Context).

get_tree(String) ->
    {ok, Tokens, _Endline} = calc_lexer:string(String),
    {ok, Tree} = calc_parser:parse(Tokens),
    Tree.

matches(A, Context) when is_number(A) ->
    {A, Context};
matches(A, Context) when is_atom(A) ->
    get_var(A, Context);
matches('true', Context) ->
    {true, Context};
matches('false', Context) ->
    {false, Context};
matches({'+', A, B}, Context) ->
    do(A, B, Context, fun(C, D) -> C + D end);
matches({'-', A, B}, Context) ->
    do(A, B, Context, fun(C, D) -> C - D end);
matches({'*', A, B}, Context) ->
    do(A, B, Context, fun(C, D) -> C * D end);
matches({'/', A, B}, Context) ->
    do(A, B, Context, fun(C, D) -> C / D end);
matches({'%', A, B}, Context) ->
    do(A, B, Context, fun(C, D) -> C rem D end);

matches({'<', A, B}, Context) ->
    do(A, B, Context, fun(C, D) -> C < D end);
matches({'<=', A, B}, Context) ->
    do(A, B, Context, fun(C, D) -> C =< D end);
matches({'==', A, B}, Context) ->
    do(A, B, Context, fun(C, D) -> C =:= D end);
matches({'>=', A, B}, Context) ->
    do(A, B, Context, fun(C, D) -> C >= D end);
matches({'>', A, B}, Context) ->
    do(A, B, Context, fun(C, D) -> C > D end);
matches({'!=', A, B}, Context) ->
    do(A, B, Context, fun(C, D) -> C =/= D end);

matches({'and', A, B}, Context) ->
    First = matches(A, Context),
    case as_boolean(First) of
        true -> {matches(B, Context), Context};
        false -> {First, Context}
    end;

matches({'or', A, B}, Context) ->
    First = matches(A, Context),
    case as_boolean(First) of
        true -> {First, Context};
        false -> {matches(B, Context), Context}
    end;

matches({'(', A}, Context) ->
    {matches(A, Context), Context};

matches({'=', A, B}, Context) ->
    {Value, _} = matches(B, Context),
    {ok, NewContext} = match_var(A, Value, Context),
    {Value, NewContext};
matches(Exp, Context) -> throw({invalid_expression, Context, ["invalid expresion", Exp]}).

do(A, B, Context, Fun) ->
    {Left, _} = matches(A, Context),
    {Right, _} = matches(B, Context),
    {Fun(Left, Right), Context}.

as_boolean(Val) when is_boolean(Val) -> Val;
as_boolean(Val) when is_number(Val) -> Val > 0.

match_var(Key, Value, Context) ->
    case dict:is_key(Key, Context) of
        % TODO: do not throw if the value is the same
        true -> throw({badmatch, Context, ["Variable already defined", Key, Value]});
        false -> {ok, dict:store(Key, Value, Context)}
    end.

get_var(Key, Context) ->
    case dict:is_key(Key, Context) of
            true -> {dict:fetch(Key, Context), Context};
            false -> throw({var_not_found, Context, ["Variable not found", Key]})
    end.
