:- module(list_util, [
  every/2,
  fill/3,
  goal_bool/2,
  list_last/2,
  list_pred_first/3,
  list_without/3,
  replace/4,
  some/2
]).

:- use_module(library(lists)).
:- use_module(library(reif)). % for if_ and tfilter

% This succeeds if every element in the given list satisfies Predicate.
:- meta_predicate every(1, ?).
every(Predicate, List) :- maplist(Predicate, List).

% This creates a list containing N copies of E
% (from Discord by @adamcrussell).
clone(X, X).
fill(N, E, L) :- length(L, N), maplist(clone(E), L).

% This is often needed with using predicates in the "reif" library.
% It sets Bool to true or false based on whether Goal succeeds or fails.
goal_bool(Goal, Bool) :- Goal -> Bool = true; Bool = false.

% This relates a list to its last element.
list_last([], []).
list_last(List, Last) :-
  length(List, Length),
  nth1(Length, List, Last).

% This relates a list and a predicate to the
% first element in the list that satisfies the predicate.
list_pred_first([], _, []).
list_pred_first([H|T], Pred, Element) :-
  Goal =.. [Pred, H],
  ( call(Goal) ->
    Element = H
  ; list_pred_first(T, Pred, Element)
  ).

% This relates the list L0 to the list L
% which does not contain any elements matching E.
list_without(L0, E, L) :- tfilter(dif(E), L0, L).

% This creates a new list from an existing list by copying it
% and replacing the element at a given zero-based index with a new value.
% The first argument is the existing list.
% The second argument is the index of the element to be replaced.
% The third argument is the new value to be used at that index.
% The fourth argument is a variable to be set to the new list.
% For example, replace([a, b, c], 1, d, L) sets L to [a, d, c].
replace([], _, _, []).
replace([_|T], 0, Value, [Value|T]) :- !.
replace([H|T], Index, Value, [H|R]) :-
  Index > 0,
  I is Index - 1,
  replace(T, I, Value, R).

% This succeeds if at least one element in the given list satisfies Predicate.
:- meta_predicate some(1, ?).
some(_, []) :- fail.
some(Predicate, [H|T]) :-
  (call(Predicate, H) ->
    true
  ; some(Predicate, T)
  ).
