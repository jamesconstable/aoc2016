:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).

read_input(Grammar) :- phrase_from_stream(Grammar, user_input).
