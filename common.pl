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

%% rotate_list(?Distance, ?List, ?Rotated)
%  Rotated is the forwards rotation of List by Distance. For example,
%  rotate_list(1, [a, b, c], X) -> X = [c, a, b]. Can be used in any mode,
%  though CLP(FD) membership constraints may be required if Distance is unbound.
rotate_list(Distance, List, Rotated) :-
  length(List, L),
  N #= Distance mod L,
  split_at(N, List, A, B),
  append(B, A, Rotated), !.

%% split_at(?Index, ?List, ?Init, ?Tail)
%  List is the concatenation of Init and Tail, where Init has length Index.
split_at(0, Xs, [], Xs).
split_at(N, [X|Xs], [X|A], B) :- N #> 0, N1 #= N-1, split_at(N1, Xs, A, B).

%% rows_cols(+Rows, -Cols) is det
%% rows_cols(-Rows, +Cols) is det
%  Rows is the transpose of Cols, where both are lists of lists.
rows_cols(Rows, []) :- maplist(=([]), Rows), !.
rows_cols(Rows, [Col|Cols]) :-
  maplist([[C|Cs], C, Cs]>>true, Rows, Col, Rows1),
  rows_cols(Rows1, Cols).

%% get_default_assoc(+Key, +Default, +Assoc, -Value) is det
%  Value is the mapping of Key in Assoc, or Default if Assoc does not have a
%  mapping for Key.
get_default_assoc(K, _, Assoc, V) :- get_assoc(K, Assoc, V).
get_default_assoc(K, Default, Assoc, Default) :- \+ get_assoc(K, Assoc, _).

