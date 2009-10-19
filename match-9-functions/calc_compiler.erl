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

get_ast1(String, ModuleName) ->
    Body = matches(get_tree(String)),
    Function = function(solve, 3, 0, [function_body([], 3, [Body])]),
    module(ModuleName, Function).

get_ast(String, ModuleName) ->
    Ast = matches(get_tree(String)),
    module(ModuleName, Ast).

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

matches({integer, _, _} = Ast) -> Ast;
matches({float, _, _} = Ast) -> Ast;
matches({atom, _, _} = Ast) -> Ast;
matches({var, _, _} = Ast) -> Ast;
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

matches({Line, fn, {_, '(', Args}, Body}) ->
    function(solve, Line, length(Args), matches_body(Body, Args));
matches(_) -> error.

matches_body({Line, '{', A}, Args) -> [function_body([matches(Arg) || Arg <- Args], Line, [matches(A)])].

