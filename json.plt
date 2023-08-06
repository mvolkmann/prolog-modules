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
  Expected = "[\"foo\",\"bar\",\"baz\"]",
  V = [foo, bar, baz],
  phrase(json(V), Actual).

test_pair(Expected, Actual) :-
  Expected = "\"red\": \"stop\"",
  phrase(json(red-stop), Actual).

test_pairs(Expected, Actual) :-
  Expected = "{ \"red\": \"stop\", \"green\": \"go\", \"yellow\": \"yield\" }",
  V = [red-stop, green-go, yellow-yield],
  format("test_pairs: V = ~w~n", [V]),
  phrase(json(V), Actual).

test_string(Expected, Actual) :-
  Expected = "\"some text\"",
  V = "some text",
  phrase(json(V), Actual).

test_structure(Expected, Actual) :-
  Expected = "{\"_functor\": \"a/2\", \"_args\": [\"b\",\"c\"]}",
  V = a(b, c),
  phrase(json(V), Actual).

test :-
  run_tests([
    user:test_atom,
    user:test_integer,
    user:test_list,
    user:test_pair,
    % user:test_pairs,
    user:test_string,
    user:test_structure
  ]),
  halt.
