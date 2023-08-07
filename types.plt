% To run only these tests, enter `scry -g test,halt types.plt`
% where `scry` is your alias for `scryer-prolog`.

:- module(types_test, [
  is_list_not_chars1/2,
  is_list_not_chars2/2,
  is_list_not_chars3/2,
  is_list_of_pairs1/2,
  is_list_of_pairs2/2,
  is_list_of_pairs3/2,
  is_pair1/2,
  is_pair2/2,
  test/0
]).

:- use_module(unit_test).
:- initialization(consult(types)).

is_list_not_chars1(Expected, Actual) :-
  Expected = true,
  V = [],
  goal_bool(types:is_list_not_chars(V), Actual).

is_list_not_chars2(Expected, Actual) :-
  Expected = true,
  V = [foo, bar, baz],
  goal_bool(types:is_list_not_chars(V), Actual).

is_list_not_chars3(Expected, Actual) :-
  Expected = false,
  V = [a, b, c],
  goal_bool(types:is_list_not_chars(V), Actual).

is_list_of_pairs1(Expected, Actual) :-
  Expected = false,
  V = [],
  goal_bool(types:is_list_of_pairs(V), Actual).

is_list_of_pairs2(Expected, Actual) :-
  Expected = true,
  V = [k1-v1, k2-v2],
  goal_bool(types:is_list_of_pairs(V), Actual).

is_list_of_pairs3(Expected, Actual) :-
  Expected = false,
  V = [k1-v1, v2],
  goal_bool(types:is_list_of_pairs(V), Actual).

is_pair1(Expected, Actual) :-
  Expected = true,
  Actual = true,
  V = key-value,
  goal_bool(types:is_pair(V), Actual).

is_pair2(Expected, Actual) :-
  Expected = false,
  V = [foo, bar],
  goal_bool(types:is_pair(V), Actual).

test :-
  run_tests([
    is_list_not_chars1,
    is_list_not_chars2,
    is_list_not_chars3,
    is_list_of_pairs1,
    is_list_of_pairs2,
    is_list_of_pairs3,
    is_pair1,
    is_pair2
  ]).
