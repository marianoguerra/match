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
    ok.
