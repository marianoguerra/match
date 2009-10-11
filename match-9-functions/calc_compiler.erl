-module(calc_compiler).
-compile(export_all).

module(Name, Ast) ->
    module(Name, Ast, 1).

module(Name, Ast, EofLine) ->
    [{attribute,1,module,Name},
        {attribute,1,compile,export_all},
        Ast, {eof, EofLine}].

function(Name, Line, Arity, Ast) ->
    {function, Line, Name, Arity, Ast}.

function_body(Pattern, Line, Body) ->
    {clause, Line, Pattern, [], Body}.

integer(Line, Value) ->
    {integer, Line, Value}.

float(Line, Value) ->
    {float, Line, Value}.

atom(Line, Value) ->
    {atom, Line, Value}.

% the code below is from reia (I would have written the same :P)
% changes only on == and !=
forms('*' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('/' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('%', Line, Ast1, Ast2) ->
  {op, Line, 'rem', Ast1, Ast2};

%% Addition
forms('+' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('-' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};

%% Boolean operators
forms('and', Line, Ast1, Ast2) ->
  {op, Line, 'andalso', Ast1, Ast2};
forms('or', Line, Ast1, Ast2) ->
  {op, Line, 'orelse', Ast1, Ast2};

%% Comparison operators
forms('==', Line, Ast1, Ast2) ->
  {op, Line, '=:=', Ast1, Ast2};
forms('!=', Line, Ast1, Ast2) ->
  {op, Line, '=/=', Ast1, Ast2};
forms('<' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('>' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('>=' = Op, Line, Ast1, Ast2) ->
  {op, Line, Op, Ast1, Ast2};
forms('<=', Line, Ast1, Ast2) ->
  {op, Line, '=<', Ast1, Ast2}.
% until here code from reia

% build a function that receives no values and returns an integer
test(Line, ModuleName, FunctionName, Value) ->
    I = integer(Line, Value),
    B = function_body([], Line, [I]),
    F = function(FunctionName, Line, 0, [B]),
    M = module(ModuleName, F),
    M.

get_ast(String, ModuleName) ->
    Body = matches(get_tree(String)),
    Function = function(solve, 3, 0, [function_body([], 3, [Body])]),
    module(ModuleName, Function).

get_code(Ast) ->
    {ok, _, Code} = compile:forms(Ast),
    Code.

compile(ModuleName, Ast) ->
    {module, Module} = code:load_binary(ModuleName, ModuleName, get_code(Ast)),
    Module.

to_erlang(String, ModuleName) ->
    Ast = get_ast(String, ModuleName),
    erl_prettypr:format(erl_syntax:form_list(Ast)).

solve(String) ->
    solve(String, module).

solve(String, ModuleName) ->
    compile(ModuleName, get_ast(String, ModuleName)).

get_tree(String) ->
    {ok, Tokens, _Endline} = calc_lexer:string(String),
    {ok, Tree} = calc_parser:parse(Tokens),
    Tree.

matches({Line, A}) when is_integer(A) -> integer(Line, A);
matches({Line, A}) when is_float(A) -> float(Line, A);
matches({Line, 'true'}) -> atom(Line, true);
matches({Line, 'false'}) -> atom(Line, false);
matches({Line, '+' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({Line, '-' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({Line, '*' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({Line, '/' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({Line, '%' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));

matches({Line, '<' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({Line, '<=' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({Line, '==' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({Line, '>=' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({Line, '>' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({Line, '!=' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));

matches({Line, 'and' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({Line, 'or' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));

matches({_Line, '(', A}) -> matches(A);
matches(_) -> error.

