-module(build_calc).
-export([build/0, test/0]).

build() ->
    leex:file(calc_lexer),
    yecc:file(calc_parser),
    compile:file(calc_lexer),
    compile:file(calc_parser),
    compile:file(calc_compiler),
    compile:file(calc_interpreter),
    ok.

test() ->
    {1 + 2 - 3 , _} = calc_interpreter:solve("1 + 2 - 3"),
    {1 + 2 + 3 , _} = calc_interpreter:solve("1 + 2 + 3"),
    {1 - 2 - 3 , _} = calc_interpreter:solve("1 - 2 - 3"),
    {0.0 , _} = calc_interpreter:solve("1.0 + 2 - 3"),
    {6.0 , _} = calc_interpreter:solve("1 + 2.0 + 3"),
    {-4.0 , _} = calc_interpreter:solve("1 - 2.0 - 3"),
    {3.1 , _} = calc_interpreter:solve("1.0 + 2.1"),
    {7 , _} = calc_interpreter:solve("1 + 2 * 3"),
    {10 , _} = calc_interpreter:solve("2 * 3 + 4"),
    {7.1 , _} = calc_interpreter:solve("1.1 + 2 * 3"),
    {10.2 , _} = calc_interpreter:solve("2 * 3.1 + 4"),
    {1 , _} = calc_interpreter:solve("11 % 2"),
    {2 , _} = calc_interpreter:solve("1 + 11 % 2"),
    {11 , _} = calc_interpreter:solve("11 % 2 + 10"),
    {14 , _} = calc_interpreter:solve("2 * (3 + 4)"),
    {19 , _} = calc_interpreter:solve("2 * (3 + 4) + 5"),
    {30.0 , _} = calc_interpreter:solve("2 * (3 + 4 / (2 + 1)) * 3 + 4"),
    {true , _} = calc_interpreter:solve("2 + 2 == 2 * 2"),
    {true , _} = calc_interpreter:solve("2 * (3 + 4 / (2 + 1)) * 3 + 4 == 30.0"),
    {true , _} = calc_interpreter:solve("1 < 2 and true"),
    {false , _} = calc_interpreter:solve("true and false"),
    {true , _} = calc_interpreter:solve("false or true or 9"),
    {7 , _} = calc_interpreter:solve("0 or 1 + 2 * 3"),
    {true , _} = calc_interpreter:solve("0 < 2 or 1 + 2 * 3"),
    {9 , _} = calc_interpreter:solve("false or (1 + 2) * 3"),
    ok.
