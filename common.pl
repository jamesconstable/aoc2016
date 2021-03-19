:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).

read_input(Grammar) :- phrase_from_stream(Grammar, user_input).

%% rle(+List, -RunLengths) is det
%  RunLengths is the run-length encoding of List, expressed in pairs.
%  For example, rle([a, a, b, c, c, c, a], X) -> X = [2-a, 1-b, 3-c, 1-a].
rle([], []).
rle(Xs, [L-E|Rs]) :- split_eqs(Xs, [E|Es], T), length([E|Es], L), rle(T, Rs).

%% split_eqs(+List, -Eqs, -Rest) is det
%  List is the concatenation of Eqs and Rest, where all elements of Eqs are
%  unifiable.
split_eqs([X], [X], []).
split_eqs([X, X|Xs], [X|Eqs], Rest) :- split_eqs([X|Xs], Eqs, Rest).
split_eqs([X1, X2|Xs], [X1], [X2|Xs]) :- X1 \= X2.

%% count(:Pred, +Xs, -N) is det
%  N is the number of elements in Xs that satisfy Pred.
count(_, [], 0).
count(Pred, [X|Xs], N1) :- call(Pred, X), !, count(Pred, Xs, N), N1 #= N + 1.
count(Pred, [_|Xs], N) :- count(Pred, Xs, N).

