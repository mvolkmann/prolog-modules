% To run all unit tests in this directory, enter `scry -g all unit_test.pl`
:- module(unit_test, [all/0, goal_bool/2, run_tests/1]).
:- use_module(library(clpz)).
:- use_module(library(files)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(strings).

% :- dynamic(user:test/0).

all :-
  directory_files(".", Files),
  sort(Files, Sorted),
  maplist(process, Sorted),
  halt.

% This sets Bool to true or false based on whether Goal succeeds or fails.
% Many tests need this functionality.
:- meta_predicate goal_bool(0). % no arguments will be passed to Goal
goal_bool(Goal, B) :- call(Goal) -> B = true; B = false.

is_test(File) :-
  filename_extension(File, _, Extension),
  Extension == "plt".

process(File) :-
  ( is_test(File) ->
    format("running ~s ...~n", [File]),
    atom_chars(Atom, File),
    consult(Atom),
    call(user:test)
    % retract(user:test)
  ; true
  ).

message(Name, Expected, Actual, Msg) :-
  ( Actual == Expected ->
    Msg = ""
  ; phrase(format_(
      % "~s expected ~w but was ~w",
      "~s expected ~s but was ~s",
      [Name, Expected, Actual]
    ), Msg)
  ).

report_count(Prefix, Count, Word) :-
  (Count #= 1 -> Noun = "test"; Noun = "tests"),
  format("~s~d ~s ~s~n", [Prefix, Count, Noun, Word]).

run_test(Module, Test, Passed0, Passed) :-
  call(Module:Test, Expected, Actual),
  predicate_namespace_name(Test, _, Name),
  message(Name, Expected, Actual, Msg),
  length(Msg, Length),
  (Length #= 0 ->
    Passed #= Passed0 + 1
  ; format("~s~n", [Msg]),
    Passed #= Passed0
  ).

:- meta_predicate run_tests(:).
% Module will capture the namespace of "Tests"
% which when unspecified will be "user".
run_tests(Module:Tests) :-
  foldl(run_test(Module), Tests, 0, Passed),
  length(Tests, Length),
  (Passed #= Length -> Prefix = "All "; Prefix = ""),
  report_count(Prefix, Passed, "passed"),
  Failed #= Length - Passed,
  (Failed #= 0 ->
    true
  ; report_count("", Failed, "failed")
  ).

test :- true.
