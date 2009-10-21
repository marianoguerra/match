-module(match).
-compile(export_all).

module(Name, Ast) ->
    module(Name, Ast, 1).

module(Name, Ast, EofLine) ->
    [{attribute,1,module,Name},
        {attribute,1,compile,export_all}] ++ Ast ++ [{eof, EofLine}].

function(Name, Line, Arity, Ast) ->
    {function, Line, Name, Arity, Ast}.

function_body(Pattern, Line, Body) ->
    {clause, Line, Pattern, [], Body}.

forms('=', Line, Ast1, Ast2) ->
  {match, Line, Ast1, Ast2};
forms('not' = Op, Line, Ast1, nil) ->
  {op, Line, Op, Ast1};
forms('-' = Op, Line, Ast1, nil) ->
  {op, Line, Op, Ast1};
forms('~', Line, Ast1, nil) ->
  {op, Line, 'bnot', Ast1};
forms('!', Line, Ast1, Ast2) ->
  {op, Line, 'bor', Ast1, Ast2};
forms('&', Line, Ast1, Ast2) ->
  {op, Line, 'band', Ast1, Ast2};
forms('^', Line, Ast1, Ast2) ->
  {op, Line, 'bxor', Ast1, Ast2};
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

get_ast(From, String, ModuleName) ->
    Ast = lists:map(fun(Line) -> matches(Line) end, get_tree(From, String)),
    module(ModuleName, Ast).

get_code(Ast) ->
    {ok, _, Code} = compile:forms(Ast),
    Code.

compile(ModuleName, Ast) ->
    {module, Module} = code:load_binary(ModuleName, ModuleName, get_code(Ast)),
    Module.

to_erlang(From, String, ModuleName) ->
    Ast = get_ast(From, String, ModuleName),
    erl_prettypr:format(erl_syntax:form_list(Ast)).

from_erlang(Name) ->
    {ok, Content} = file:read_file(Name),
    Program = binary_to_list(Content),
    {ok,Scanned,_} = erl_scan:string(Program),
    {ok,Parsed} = erl_parse:parse_form(Scanned),
    Parsed.

build(From, String) ->
    build(From, String, module).

build(From, String, ModuleName) ->
    compile(ModuleName, get_ast(From, String, ModuleName)).

get_tree(From, String) ->
    Tokens = get_lex(From, String),
    {ok, Tree} = parser:parse(Tokens),
    Tree.

get_lex(string, String) ->
    {ok, Tokens, _Endline} = lexer:string(String),
    Tokens;
get_lex(file, Name) ->
    {ok, Content} = file:read_file(Name),
    Program = binary_to_list(Content),
    {ok, Tokens, _Endline} = lexer:string(Program),
    Tokens.

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

matches({Line, '!' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({Line, '&' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({Line, '^' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));

matches({Line, 'and' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({Line, 'or' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));

matches({Line, 'not' = Op, A}) -> forms(Op, Line, matches(A), nil);
matches({Line, '~' = Op, A}) -> forms(Op, Line, matches(A), nil);
matches({_Line, '+', A}) -> matches(A);
matches({Line, '-' = Op, A}) -> forms(Op, Line, matches(A), nil);
matches({_Line, '(', A}) -> matches(A);

matches({Line, callatom, Atom, Args}) ->
    {call, Line, {atom, Line, Atom}, lists:map(fun(Arg) -> matches(Arg) end, Args)};
matches({Line, call, A, Args}) ->
    {call, Line, matches(A), lists:map(fun(Arg) -> matches(Arg) end, Args)};
matches({Line, '=' = Op, A, B}) -> forms(Op, Line, matches(A), matches(B));
matches({Line, fn, {_, '(', Args}, Body}) ->
    {'fun', Line, match_fun_body(Body, Args)};
matches({Line, fun_def, Name, {_, fn, {_, '(', Args}, Body}}) ->
    function(Name, Line, length(Args), match_function_body(Body, Args));
matches(Exp) -> {error, Exp}.

match_fun_body(Body, Args) ->
    {clauses, match_function_body(Body, Args)}.

match_function_body({Line, '{', A}, Args) ->
    [function_body([matches(Arg) || Arg <- Args], Line, lists:map(fun(L) -> matches(L) end, A))].
