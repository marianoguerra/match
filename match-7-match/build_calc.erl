-module(build_calc).
-export([build/0, test/0]).

build() ->
    leex:file(calc_lexer),
    yecc:file(calc_parser),
    compile:file(calc_lexer),
    compile:file(calc_parser),
    compile:file(calc),
    ok.

test() ->
    0 = calc:solve("1 + 2 - 3"),
    6 = calc:solve("1 + 2 + 3"),
    -4 = calc:solve("1 - 2 - 3"),
    0.0 = calc:solve("1.0 + 2 - 3"),
    6.0 = calc:solve("1 + 2.0 + 3"),
    -4.0 = calc:solve("1 - 2.0 - 3"),
    3.1 = calc:solve("1.0 + 2.1"),
    7 = calc:solve("1 + 2 * 3"),
    10 = calc:solve("2 * 3 + 4"),
    7.1 = calc:solve("1.1 + 2 * 3"),
    10.2 = calc:solve("2 * 3.1 + 4"),
    1 = calc:solve("11 % 2"),
    2 = calc:solve("1 + 11 % 2"),
    11 = calc:solve("11 % 2 + 10"),
    14 = calc:solve("2 * (3 + 4)"),
    19 = calc:solve("2 * (3 + 4) + 5"),
    30.0 = calc:solve("2 * (3 + 4 / (2 + 1)) * 3 + 4"),
    true = calc:solve("2 + 2 == 2 * 2"),
    true = calc:solve("2 * (3 + 4 / (2 + 1)) * 3 + 4 == 30.0"),
    true = calc:solve("1 < 2 and true"),
    false = calc:solve("true and false"),
    true = calc:solve("false or true or 9"),
    7 = calc:solve("0 or 1 + 2 * 3"),
    true = calc:solve("0 < 2 or 1 + 2 * 3"),
    9 = calc:solve("false or (1 + 2) * 3"),
    ok.
