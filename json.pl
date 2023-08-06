:- module(json, [
  is_list_not_chars/1,
  is_pair/1,
  json/3,
  report/1,
  structure_functor/2
]).

:- use_module(library(dcgs)).
:- use_module(library(format)).
:- use_module(library(lists)). % for maplist
:- use_module(library(si)).
:- use_module(strings).

% Use this line to suppress logging.
log(_, _).
% Use this line to enable logging.
% log(Format, Arguments) :- format(Format, Arguments).

% Technically a double-quoted string is a list of character atoms,
% but we want to consider that to be a string.
% Otherwise we could use the `list_si` predicate.
is_list_not_chars(X) :-
  list_si(X),
  ( X = [] ->
    true
  ; \+ chars_si(X)
  ).

is_pair(X) :-
  functor(X, Name, Arity),
  ( Name == (-), Arity == 2 ->
    true
  ; false
  ).

% To test this, enter something like the following and see the value of A.
% V = foo, phrase(json(V), C), atom_chars(A, C).
% A = '"foo"'
json(Atom) -->
  "\"",
  {
    atom_si(Atom),
    log("json Atom: Atom = ~w~n", [Atom]),
    atom_chars(Atom, Chars),
    log("json Atom: Chars = ~w~n", [Chars])
  },
  seq(Chars),
  "\"".
  
% To test this, enter something like the following and see the value of C.
% V = 123, phrase(json(V), C).
% C = "123"
json(Integer) -->
  {
    integer_si(Integer),
    log("json Integer: Integer = ~w~n", [Integer]),
    number_chars(Integer, Chars),
    log("json Integer: Chars = ~w~n", [Chars])
  },
  seq(Chars).
  
% To test this, enter something like the following and see the value of A.
% V = [foo, bar, baz], phrase(json(V), C), atom_chars(A, C).
% A = '["foo","bar","baz"]'
json(List) -->
  "[",
  {
    is_list_not_chars(List),

    log("json List: List = ~w~n", [List]),
    values_json(List, Json),
    log("json List: Json = ~w~n", [Json])
  },
  seq(Json),
  "]".
  
% TODO: This is not working yet!
% To test this, enter something like the following and see the value of A.
% V = foo-bar, phrase(json(V), C), atom_chars(A, C).
% A = '"foo": "bar"'
% TODO: Check for list containing only pairs and generate a JSON object.
json(Pair) -->
  {
    is_pair(Pair),
    format("json Pair: Pair = ~w~n", [Pair]),
    Key-Value = Pair,
    format("json Pair: Key = ~w~n", [Key]),
    format("json Pair: Value = ~w~n", [Value]),
    % value_json(Value, Json)
    % once(phrase(json(Value), Json))
    Json = '"barking"'
  },
  "\"", seq(Key), "\": ", seq(Json).

% To test this, enter something like the following and see the value of A.
% V = a(b,c), phrase(json(V), C), atom_chars(A, C).
% A = '{"_functor": "a/2", "_args": ["b","c"]}'
json(Structure) -->
  "{",
  {
    % TODO: Why are all these checks necessary?
    \+ atom_si(Structure),
    \+ chars_si(Structure), % verifies Structure is not chars
    \+ is_list_not_chars(Structure),
    \+ is_pair(Structure),

    log("json: Structure = ~w~n", [Structure]),
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
  "\"", String, "\"",
  {
    log("json String: String = ~w~n", [String]),
    chars_si(String),
    log("json: String = ~w~n", [String])
  }.
  
% For debugging
report(Structure) :-
  functor(Structure, Name, Arity),
  log("Name = ~w~n", [Name]),
  log("Arity = ~w~n", [Arity]),
  arg(1, Structure, Arg),
  log("First Arg = ~w~n", [Arg]),
  Structure =.. List,
  log("List = ~w~n", [List]).

% This relates a structure to its functor.
% For example, `structure_functor(a(b, c), F)`
% will set F to "a/2".
structure_functor(Structure, Functor) :-
  functor(Structure, Name, Arity),
  log("structure_functor: calling atom_chars with Name = ~w~n", [Name]),
  atom_chars(Name, NameC),
  number_chars(Arity, ArityC),
  append(NameC, "/", Functor0),
  append(Functor0, ArityC, Functor).

value_json(Value, Json) :-
  log("value_json: Value = ~w~n", [Value]),
  once(phrase(json(Value), Json)),
  log("value_json: Json = ~w~n", [Json]).

values_json(Values, Json) :-
  log("values_json: Values = ~w~n", [Values]),
  % Convert Values list to JSON list.
  maplist(value_json, Values, Jsons),
  log("values_json: Jsons = ~w~n", [Jsons]),
  % Create string that is a comma-separated list of the JSON values.
  string_list(Json, ',', Jsons),
  log("values_json: Json = ~w~n", [Json]).
