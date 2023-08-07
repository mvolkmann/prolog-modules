:- use_module(library(format)).
:- use_module(unit_test).
:- initialization(consult(json)).

test_atom(Expected, Actual) :-
  Expected = "\"foo\"",
  V = foo,
  phrase(json(V), Actual).

test_integer(Expected, Actual) :-
  Expected = "123",
  V = 123,
  phrase(json(V), Actual).

test_list(Expected, Actual) :-
  Expected = "[\"foo\", \"bar\", \"baz\"]",
  V = [foo, bar, baz],
  phrase(json(V), Actual).

test_list_of_pairs(Expected, Actual) :-
  Expected = "{\"red\": \"stop\", \"green\": \"go\", \"yellow\": \"yield\"}",
  V = [red-stop, green-go, yellow-yield],
  phrase(json(V), Actual).

test_pair(Expected, Actual) :-
  Expected = "\"red\": \"stop\"",
  phrase(json(red-stop), Actual).

test_string(Expected, Actual) :-
  Expected = "\"some text\"",
  V = "some text",
  phrase(json(V), Actual).

test_structure(Expected, Actual) :-
  Expected = "{\"_functor\": \"a/2\", \"_args\": [\"b\", \"c\"]}",
  V = a(b, c),
  phrase(json(V), Actual).

test :-
  run_tests([
    test_atom,
    test_integer,
    test_pair,
    test_list,
    test_list_of_pairs,
    test_string,
    test_structure
  ]),
  halt.
