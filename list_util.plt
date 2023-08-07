% To run this, enter `scry -g test list_util.plt`
% where `scry` is your alias for `scryer-prolog`.

:- use_module(library(clpz)).
:- use_module(unit_test).
:- initialization(consult(list_util)).

is_even(N) :- mod(N, 2) =:= 0.

every1(Expected, Actual) :-
  Expected = true,
  L = [2, 6, 8],
  goal_bool(list_util:every(is_even, L), Actual).

every2(Expected, Actual) :-
  Expected = false,
  L = [2, 5, 8],
  goal_bool(list_util:every(is_even, L), Actual).

fill1(Expected, Actual) :-
  Expected = [],
  fill(0, a, Actual).

fill2(Expected, Actual) :-
  Expected = [a],
  fill(1, a, Actual).

fill3(Expected, Actual) :-
  Expected = [a, a, a],
  fill(3, a, Actual).

first1(Expected, Actual) :-
  Expected = [],
  L = [],
  first(var, L, Actual).

first2(Expected, Actual) :-
  Expected = [],
  L = [foo, bar, baz],
  first(var, L, Actual).

first3(Expected, Actual) :-
  Expected = Bar,
  L = [foo, Bar, baz],
  first(var, L, Actual).

some1(Expected, Actual) :-
  Expected = true,
  L = [3, 6, 9],
  goal_bool(list_util:some(is_even, L), Actual).

some2(Expected, Actual) :-
  Expected = false,
  L = [3, 5, 9],
  goal_bool(list_util:some(is_even, L), Actual).

list_last1(Expected, Actual) :-
  Expected = [],
  list_last([], Actual).

list_last2(Expected, Actual) :-
  Expected = baz,
  list_last([foo, bar, baz], Actual).

list_without1(Expected, Actual) :-
  Expected = [foo, baz],
  list_without([foo, bar, baz], bar, Actual).

list_without2(Expected, Actual) :-
  Expected = [foo, bar, baz],
  list_without([foo, bar, baz], missing, Actual).

list_without3(Expected, Actual) :-
  Expected = [],
  list_without([], foo, Actual).

replace1(Expected, Actual) :-
  Expected = [],
  replace([], 0, x, Actual).

replace2(Expected, Actual) :-
  Expected = [d, b, c],
  replace([a, b, c], 0, d, Actual).

replace3(Expected, Actual) :-
  Expected = [a, d, c],
  replace([a, b, c], 1, d, Actual).

test :-
  run_tests([
    every1,
    every2,
    fill1,
    fill2,
    fill3,
    first1,
    first2,
    first3,
    some1,
    some2,
    list_last1,
    list_last2,
    list_without1,
    list_without2,
    list_without3,
    replace1,
    replace2,
    replace3
  ]).
