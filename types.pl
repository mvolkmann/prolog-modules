:- module(types, [
  is_list_not_chars/1,
  is_list_of_pairs/1,
  is_pair/1
]).

:- use_module(library(lists)). % for length
:- use_module(library(si)).
:- use_module(list_util). % for every

% Technically a double-quoted string is a list of character atoms,
% but we want to consider that to be a string.
% Otherwise we could use the `list_si` predicate.
is_list_not_chars(X) :-
  list_si(X),
  ( X = [] ->
    true
  ; \+ chars_si(X)
  ).

is_list_of_pairs(X) :-
  list_si(X),
  length(X, Length),
  Length > 0,
  every(is_pair, X).

is_pair(X) :-
  functor(X, Name, Arity),
  ( Name == (-), Arity == 2 ->
    true
  ; false
  ).

