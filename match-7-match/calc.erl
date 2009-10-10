-module(calc).
-export([solve/1, solve/2, get_tree/1, interpreter/0]).

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
matches('true', Context) ->
    {true, Context};
matches('false', Context) ->
    {false, Context};
matches(A, Context) when is_atom(A) ->
    get_var(A, Context);
matches({'+', A, B}, Context) ->
    do(A, B, Context, fun(C, D) -> C + D end);
matches({'-', A, B}, Context) ->
    do(A, B, Context, fun(C, D) -> C - D end);
matches({'*', A, B}, Context) ->
    do(A, B, Context, fun(C, D) -> C * D end);
matches({'/', A, B}, Context) ->
    do(A, B, Context, fun(C, D) -> C / D end);
matches({'%', A, B}, Context) when is_integer(A) and is_integer(B) ->
    do(A, B, Context, fun(C, D) -> C rem D end);
matches({'%', A, B}, Context) ->
    throw({error, Context, "invalid arguments for %", [A, B]});

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
    {First, _} = matches(A, Context),
    case as_boolean(First) of
        true ->
            {Second, _} = matches(B, Context),
            {Second, Context};
        false -> {First, Context}
    end;

matches({'or', A, B}, Context) ->
    {First, _} = matches(A, Context),
    case as_boolean(First) of
        true -> {First, Context};
        false ->
            {Second, _} = matches(B, Context),
            {Second, Context}
    end;

matches({'(', A}, Context) ->
    {Value, _} = matches(A, Context),
    {Value, Context};

matches({'=', A, B}, Context) ->
    {Value, _} = matches(B, Context),
    {ok, NewContext} = match_var(A, Value, Context),
    {Value, NewContext};
matches(Exp, Context) -> throw({error, Context, "invalid expresion", [Exp]}).

do(A, B, Context, Fun) ->
    {Left, _} = matches(A, Context),
    {Right, _} = matches(B, Context),
    {Fun(Left, Right), Context}.

as_boolean(Val) when is_boolean(Val) -> Val;
as_boolean(Val) when is_number(Val) -> Val > 0.

match_var(Key, Value, Context) ->
    case dict:is_key(Key, Context) of
        % TODO: do not throw if the value is the same
        true -> throw({error, Context, "Variable already defined", [Key, Value]});
        false -> {ok, dict:store(Key, Value, Context)}
    end.

get_var(Key, Context) ->
    case dict:is_key(Key, Context) of
            true -> {dict:fetch(Key, Context), Context};
            false -> throw({error, Context, "Variable not found", [Key]})
    end.

interpreter() ->
    interpreter(dict:new()).

interpreter(Context) ->
    Line = io:get_line("< "),
    try
        {Result, NewContext} = solve(Line, Context),
        io:format("> ~p~n", [Result]),
        interpreter(NewContext)
    catch {error, _ErrorContext, Message, Values} ->
        io:format("error: ~p ~p~n", [Message, Values]),
        interpreter(Context)
    end.

