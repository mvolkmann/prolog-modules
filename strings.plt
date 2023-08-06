% To run this, enter `scry -g test list_util.plt`
% where `scry` is your alias for `scryer-prolog`.

:- use_module(unit_test).
:- initialization(consult(strings)).

chars_capitalized1(Expected, Actual) :-
  Expected = "Foo",
  chars_capitalized("foo", Actual).

chars_capitalized2(Expected, Actual) :-
  Expected = "",
  chars_capitalized("", Actual).

filename_extension1(Expected, Actual) :-
  Expected = ["foo", "bar"],
  S = "foo.bar",
  filename_extension(S, Filename, Extension),
  Actual = [Filename, Extension].

predicate_namespace_name1(Expected, Actual) :-
  Expected = ["foo", "bar"],
  predicate_namespace_name(foo:bar, Namespace, Name),
  Actual = [Namespace, Name].

repeat1(Expected, Actual) :-
  Expected = "",
  repeat(a, 0, Actual).

split1(Expected, Actual) :-
  Expected = ["foo", "bar,baz"],
  S = "foo,bar,baz",
  split(S, ',', Prefix, Suffix),
  Actual = [Prefix, Suffix].

string_list(Expected, Actual) :-
  Expected = ["foo", "bar", "baz"],
  S = "foo,bar,baz",
  string_list(S, ',', Actual).

test :-
  run_tests([
    % TODO: Is there a way to automate adding the user namespace to these?
    % TODO: Is there a way to make user be the default namespace?
    user:chars_capitalized1,
    user:chars_capitalized2,
    user:filename_extension1,
    user:predicate_namespace_name1,
    user:repeat1,
    user:split1,
    user:string_list
  ]),
  halt.
