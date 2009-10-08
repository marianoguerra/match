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
     4 = calc:solve("2 + 2"),
     4 = calc:solve("6 - 2"),
     -4 = calc:solve("2 - 6"),
     6 = calc:solve("2 * 3"),
     2.0 = calc:solve("4 / 2"),
     1.2 = calc:solve("1 + 0.2"),
     1.2 = calc:solve("1.0 + 0.2"),
     1.5 = calc:solve("3 / 2.0"),
     1.5 = calc:solve("3.0 / 2.0"),
     6.0 = calc:solve("2 * 3.0"),
     6.0 = calc:solve("2.0 * 3.0"),
     2 = calc:solve("1 - 2 + 3"),
     6 = calc:solve("1 + 2 + 3"),
     0 = calc:solve("1 + 2 - 3"),
     -1 = calc:solve("1 * 2 - 3"),
     -10 = calc:solve("1 * 2 - 3 * 4"),
     11 = calc:solve("1 + 2 * 3 + 4"),
     9.0 = calc:solve("1 + 2 * 3 + 4 / 2"),
     7 = calc:solve("1 + 2 * 3"),
     1 = calc:solve("11 % 2"),
     0 = calc:solve("11.0 % 2"),
     0 = calc:solve("11 % 2.0"),
     0 = calc:solve("11.0 % 2.0"),
     ok.
