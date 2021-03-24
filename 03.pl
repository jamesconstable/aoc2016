#! /usr/bin/env swipl

% Solver for Day 3 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/3

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, read_input(triples(Ts)), count_valid(Ts, C), writeln(C).
main(['2']) :- !,
  read_input(triples(Ts)), cols_rows(Ts, Ts1), count_valid(Ts1, C), writeln(C).
main(_) :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

%% count_valid(+Triples, -Count) is det
%  Count is the number of valid triangles in Triples.
count_valid([], 0).
count_valid([T|Ts], N1) :- valid(T), !, count_valid(Ts, N), N1 #= N+1.
count_valid([T|Ts], N) :- \+ valid(T), count_valid(Ts, N).

%% valid(+Sides) is semidet
%  True if Sides are valid side-lengths for a triangle.
valid(Sides) :- msort(Sides, [A, B, C]), A + B #> C.

%% cols_rows(?Cols, ?Rows) is det
%  Given a list of 3-element lists, transposes them in groups of three.
cols_rows([], []).
cols_rows(
    [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]|T],
    [[A1, B1, C1], [A2, B2, C2], [A3, B3, C3]|T1]) :-
  cols_rows(T, T1).

% Input grammar
triples([T|Ts]) --> triple(T), blanks, triples(Ts).
triples([])     --> [].
triple([A,B,C]) --> blanks, integer(A), blanks, integer(B), blanks, integer(C).
