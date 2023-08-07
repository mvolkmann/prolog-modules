% To run only these tests, enter `scry -g test,halt json.plt`
% where `scry` is your alias for `scryer-prolog`.

:- use_module(library(format)).
:- use_module(unit_test).
:- initialization(consult(json)).

atom1(Expected, Actual) :-
  Expected = "\"foo\"",
  V = foo,
  phrase(json(V), Actual).

atom2(Expected, Actual) :-
  Expected = "true",
  V = true,
  phrase(json(V), Actual).

atom3(Expected, Actual) :-
  Expected = "false",
  V = false,
  phrase(json(V), Actual).

integer1(Expected, Actual) :-
  Expected = "123",
  V = 123,
  phrase(json(V), Actual).

list1(Expected, Actual) :-
  Expected = "[]",
  V = [],
  phrase(json(V), Actual).

list2(Expected, Actual) :-
  Expected = "[\"foo\", \"bar\", \"baz\"]",
  V = [foo, bar, baz],
  phrase(json(V), Actual).

list_of_pairs(Expected, Actual) :-
  Expected = "{\"red\": \"stop\", \"green\": \"go\", \"yellow\": \"yield\"}",
  V = [red-stop, green-go, yellow-yield],
  phrase(json(V), Actual).

pair1(Expected, Actual) :-
  Expected = "\"red\": \"stop\"",
  phrase(json(red-stop), Actual).

string1(Expected, Actual) :-
  Expected = "\"some text\"",
  V = "some text",
  phrase(json(V), Actual).

structure1(Expected, Actual) :-
  Expected = "{\"_functor\": \"a/2\", \"_args\": [\"b\", \"c\"]}",
  V = a(b, c),
  phrase(json(V), Actual).

test :-
  run_tests([
    atom1,
    atom2,
    atom3,
    integer1,
    pair1,
    list1,
    list2,
    list_of_pairs,
    string1,
    structure1
  ]).
