:- module(strings, [
    chars_capitalized/2,
    filename_extension/3,
    join/3,
    predicate_namespace_name/3,
    repeat/3,
    split/4,
    starts_upper/1,
    string_list/3
  ]).

:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(reif)). % for if_ and memberd_t
:- use_module(library(si)).

chars_capitalized([], []).
chars_capitalized([H1|T], Cap):-
  % The to_upper argument gets set to a list,
  % but we only want the first character.
  char_type(H1, to_upper([H2|_])),
  Cap = [H2|T].

% Example: filename_extension("foo.bar", F, E).
% gives F = "foo", E = "bar"
filename_extension(S, Filename, Extension) :-
  split(S, ., Filename, Extension).

predicate_namespace_name(Predicate, Namespace, Name) :-
  phrase(format_("~w", [Predicate]), S),
  if_(
    memberd_t(':', S),
    split(S, :, Namespace, Name),
    (Namespace = "", Name = S)
  ).

% This creates a string containing a given character repeated N times.
% The first two arguments must be instantiated (ground).
repeat(Char, N, S) :-
  ground(Char),
  ground(N),
  repeat_(Char, N, L),
  append(L, S).
repeat_(_, 0, []) :- !.
repeat_(Char, N, [Char|T]) :-
  N2 is N - 1,
  repeat_(Char, N2, T).

% This approach does not use DCGs.
split(S, Delimiter, Prefix, Suffix) :-
  if_(
    memberd_t(Delimiter, S),
    once(append(Prefix, [Delimiter|Suffix], S)),
    (Prefix = S, Suffix = "")
  ).

starts_upper(S) :-
  chars_si(S),
  [H|_] = S,
  char_type(H, upper). % verifies Structure is not chars

% string_list below only handles single character atom delimiters.
% This handles string delimiters of any length.
join([], _, []).
join([T], _, T).
join(L, Delimiter, R) :-
  length(L, Length),
  Length > 1,
  [H|T] = L,
  append(H, Delimiter, R0),
  join(T, Delimiter, R1),
  append(R0, R1, R).

% This relates a string to list of string parts
% obtained by splitting on a given delimiter
% which is a single character atom.
% For example:
% string_list("foo,bar,baz", ',', L).
% gives L = ["foo","bar","baz"].
% and
% string_list(S, ',', ["foo","bar","baz"]).
% gives S = "foo,bar,baz".
string_list(String, Delimiter, Parts) :-
  once(string_list_(String, Delimiter, Parts)).
string_list_("", _, []).
string_list_(String, Delimiter, Parts) :-
  if_(
    memberd_t(Delimiter, String),
    % then part
    (
      Parts = [Before|Parts0], % must be first to terminate
      % Get parts of String before and after Delimiter.
      once(append(Before, [Delimiter|After], String)),
      % Recursively process After.
      string_list_(After, Delimiter, Parts0)
    ),
    % else part
    Parts = [String]
  ).

% This approach uses DCGs.
% prefix(split(Prefix, Suffix)) --> seq(Prefix), ".", seq(Suffix).
% split(Delimiter, Prefix, Suffix) --> seq(Prefix), Delimiter, seq(Suffix).
% filename_extension(Filename, Extension) --> split(".", Filename, Extension).
