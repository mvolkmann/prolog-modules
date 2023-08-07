% To run only these tests, enter `scry -g test,halt list_util.plt`
% where `scry` is your alias for `scryer-prolog`.

:- module(list_util_test, [
  every1/2,
  every2/2,
  fill1/2,
  fill2/2,
  fill3/2,
  is_even/1,
  list_last1/2,
  list_last2/2,
  list_matching1/2,
  list_matching2/2,
  list_matching3/2,
  list_pred_first1/2,
  list_pred_first2/2,
  list_pred_first3/2,
  list_without1/2,
  list_without2/2,
  list_without3/2,
  replace1/2,
  replace2/2,
  replace3/2,
  some1/2,
  some2/2,
  test/0
]).

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
  list_util:fill(0, a, Actual).

fill2(Expected, Actual) :-
  Expected = [a],
  list_util:fill(1, a, Actual).

fill3(Expected, Actual) :-
  Expected = [a, a, a],
  list_util:fill(3, a, Actual).

list_last1(Expected, Actual) :-
  Expected = [],
  list_util:list_last([], Actual).

list_last2(Expected, Actual) :-
  Expected = baz,
  list_util:list_last([foo, bar, baz], Actual).

list_matching1(Expected, Actual) :-
  Expected = [],
  list_util:list_matching([], var, Actual).

list_matching2(Expected, Actual) :-
  Expected = [],
  L = [foo, bar, baz],
  list_util:list_matching(L, var, Actual).

list_matching3(Expected, Actual) :-
  Expected = [Bar, Qux],
  L = [foo, Bar, baz, Qux],
  list_util:list_matching(L, var, Actual).

list_pred_first1(Expected, Actual) :-
  Expected = [],
  L = [],
  list_util:list_pred_first(L, var, Actual).

list_pred_first2(Expected, Actual) :-
  Expected = [],
  L = [foo, bar, baz],
  list_util:list_pred_first(L, var, Actual).

list_pred_first3(Expected, Actual) :-
  Expected = Bar,
  L = [foo, Bar, baz],
  list_util:list_pred_first(L, var, Actual).

list_without1(Expected, Actual) :-
  Expected = [foo, baz],
  list_util:list_without([foo, bar, baz], bar, Actual).

list_without2(Expected, Actual) :-
  Expected = [foo, bar, baz],
  list_util:list_without([foo, bar, baz], missing, Actual).

list_without3(Expected, Actual) :-
  Expected = [],
  list_util:list_without([], foo, Actual).

replace1(Expected, Actual) :-
  Expected = [],
  list_util:replace([], 0, x, Actual).

replace2(Expected, Actual) :-
  Expected = [d, b, c],
  list_util:replace([a, b, c], 0, d, Actual).

replace3(Expected, Actual) :-
  Expected = [a, d, c],
  list_util:replace([a, b, c], 1, d, Actual).

some1(Expected, Actual) :-
  Expected = true,
  L = [3, 6, 9],
  goal_bool(list_util:some(is_even, L), Actual).

some2(Expected, Actual) :-
  Expected = false,
  L = [3, 5, 9],
  goal_bool(list_util:some(is_even, L), Actual).

test :-
  run_tests([
    every1,
    every2,
    fill1,
    fill2,
    fill3,
    list_last1,
    list_last2,
    list_matching1,
    list_matching2,
    list_matching3,
    list_pred_first1,
    list_pred_first2,
    list_pred_first3,
    list_without1,
    list_without2,
    list_without3,
    replace1,
    replace2,
    replace3,
    some1,
    some2
  ]).
