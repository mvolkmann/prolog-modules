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

join1(Expected, Actual) :-
  Expected = "",
  List = [""],
  join(List, "DELIM", Actual).

join2(Expected, Actual) :-
  Expected = "foo",
  List = ["foo"],
  join(List, "DELIM", Actual).

join3(Expected, Actual) :-
  Expected = "fooDELIMbarDELIMbaz",
  List = ["foo", "bar", "baz"],
  join(List, "DELIM", Actual).

predicate_namespace_name1(Expected, Actual) :-
  Expected = ["", "bar"], % no namespace
  predicate_namespace_name(bar, Namespace, Name),
  Actual = [Namespace, Name].

predicate_namespace_name2(Expected, Actual) :-
  Expected = ["foo", "bar"],
  predicate_namespace_name(foo:bar, Namespace, Name),
  Actual = [Namespace, Name].

repeat1(Expected, Actual) :-
  Expected = "",
  repeat(a, 0, Actual).

split1(Expected, Actual) :-
  Expected = ["foo", ""],
  S = "foo",
  split(S, ',', Prefix, Suffix),
  Actual = [Prefix, Suffix].

split2(Expected, Actual) :-
  Expected = ["foo", "bar,baz"],
  S = "foo,bar,baz",
  split(S, ',', Prefix, Suffix),
  Actual = [Prefix, Suffix].

string_list1(Expected, Actual) :-
  Expected = [],
  S = "",
  string_list(S, ',', Actual).

string_list2(Expected, Actual) :-
  Expected = ["foo"],
  S = "foo",
  string_list(S, ',', Actual).

string_list3(Expected, Actual) :-
  Expected = ["foo", "bar", "baz"],
  S = "foo,bar,baz",
  string_list(S, ',', Actual).

test :-
  run_tests([
    % TODO: Is there a way to automate adding the user namespace to these?
    % TODO: Is there a way to make user be the default namespace?
    chars_capitalized1,
    chars_capitalized2,
    filename_extension1,
    join1,
    join2,
    join3,
    predicate_namespace_name1,
    predicate_namespace_name2,
    repeat1,
    split1,
    split2,
    string_list1,
    string_list2,
    string_list3
  ]),
  halt.
