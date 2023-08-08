/*
I tried to use the Scryer serialization/json library,
but that requires telling it the type of everything to be serialized
rather than it determining that on its own like the implementation here does.
Also, I can't get uses of its json_chars DCG goal to terminate.
See https://github.com/mthom/scryer-prolog/discussions/1942.
*/
:- module(json, [
  json/3,
  structure_functor/2
]).

:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)). % for maplist
:- use_module(library(si)).
:- use_module(strings).
:- use_module(types).

% Use this line to suppress logging.
log(_, _).
% Use this line to enable logging.
% log(Format, Arguments) :- format(Format, Arguments).

% To test this, enter something like the following and see the value of A.
% V = foo, phrase(json(V), C), atom_chars(A, C).
% A = '"foo"'
% We do not want quotes around the Boolean values true and false.
json(true) --> "true".
json(false) --> "false".
json(Atom) -->
  "\"",
  {
    atom_si(Atom),
    Atom \== [], % treat as empty list
    log("got Atom ~w~n", [Atom]),
    atom_chars(Atom, Chars)
  },
  seq(Chars),
  "\"".
  
% To test this, enter something like the following and see the value of C.
% V = 123, phrase(json(V), C).
% C = "123"
json(Integer) -->
  {
    integer_si(Integer),
    log("got Integer ~w~n", [Integer]),
    number_chars(Integer, Chars)
  },
  seq(Chars).
  
% To test this, enter something like the following and see the value of A.
% V = [foo, bar, baz], phrase(json(V), C), atom_chars(A, C).
% A = '["foo","bar","baz"]'
json(List) -->
  {
    is_list_not_chars(List),
    \+ is_list_of_pairs(List),
    log("got List ~w~n", [List]),
    values_json(List, Json)
  },
  "[", seq(Json), "]".

% To test this, enter something like the following and see the value of A.
% V = foo-bar, phrase(json(V), J).
% J = "\"foo\": \"bar\""
json(Pair) -->
  {
    is_pair(Pair),
    log("got Pair ~w~n", [Pair]),
    Key-Value = Pair,
    atom_chars(Key, KeyChars),
    value_json(Value, Json)
  },
  "\"", seq(KeyChars), "\": ", seq(Json).

% To test this, enter something like the following and see the value of A.
% V = [red-stop, green-go, yellow-yield], phrase(json(V), C), atom_chars(A, C).
% A = '{"red": "stop", "green": "go", "yellow": "yield"}'
json(Pairs) -->
  {
    is_list_of_pairs(Pairs),
    log("got Pairs ~w~n", [Pairs]),
    values_json(Pairs, Json)
  },
  "{", seq(Json), "}".

% To test this, enter something like the following and see the value of A.
% V = a(b,c), phrase(json(V), C), atom_chars(A, C).
% A = '{"_functor": "a/2", "_args": ["b","c"]}'
json(Structure) -->
  "{",
  {
    % Rule out Structure values that are handled by other grammar rules.
    \+ atom_si(Structure),
    \+ chars_si(Structure), % verifies Structure is not chars
    \+ is_list_not_chars(Structure),
    \+ is_pair(Structure),

    log("got Structure ~w~n", [Structure]),
    Structure =.. [_|Args],
    length(Args, L),
    L > 0,
    structure_functor(Structure, Functor),
    values_json(Args, ArgsJson)
  },
  "\"_functor\": \"",
  seq(Functor),
  "\", \"_args\": [",
  seq(ArgsJson),
  "]}".  
  
% This must appear after json(Structure), but I don't know why.
% To test this, enter something like the following and see the value of A.
% V = "some text", phrase(json(V), C), atom_chars(A, C).
% A = '"some text"'
json(String) -->
  { chars_si(String) },
  % log("got String ~s~n", [String]),
  "\"", String, "\"".
  
% This relates a structure to its functor.
% For example, `structure_functor(a(b, c), F)`
% will set F to "a/2".
structure_functor(Structure, Functor) :-
  functor(Structure, Name, Arity),
  atom_chars(Name, NameC),
  number_chars(Arity, ArityC),
  append(NameC, "/", Functor0),
  append(Functor0, ArityC, Functor).

value_json(Value, Json) :-
  once(phrase(json(Value), Json)).

values_json(Values, Json) :-
  % Convert Values list to JSON list.
  maplist(value_json, Values, Jsons),
  % Create string that is a comma-separated list of the JSON values.
  % string_list(Json, ',', Jsons),
  join(Jsons, ", ", Json).
