#! /usr/bin/env swipl

% Solver for Day 13 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/13

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, read_input((integer(DFN), blanks)), part1(DFN, R), writeln(R).
main(['2']) :- !, read_input((integer(DFN), blanks)), part2(DFN, R), writeln(R).
main(_) :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(DFN, R) :- bfs(1/1, unbounded_step(DFN), =(31/39), (=), _, R).

part2(DFN, R) :-
  findall(C, bfs(1/1, bounded_step(DFN, 50), yes, (=), _, C), Cs),
  length(Cs, R).

%% take_step(+DFN, ?X/Y, ?X1/Y1) is nondet
%  X1/Y1 can be reached in a single step from X/Y in the maze defined by the
%  given DFN (designer's favourite number).
take_step(DFN, XY, XY1) :-
  take_step(XY, XY1), is_space(DFN, XY), is_space(DFN, XY1).

take_step(X/Y, X1/Y) :- X #> 0, X1 #= X - 1.
take_step(X/Y, X1/Y) :- X1 #= X + 1.
take_step(X/Y, X/Y1) :- Y #> 0, Y1 #= Y - 1.
take_step(X/Y, X/Y1) :- Y1 #= Y + 1.

unbounded_step(DFN, _, S, S1) :- take_step(DFN, S, S1).
bounded_step(DFN, MaxDepth, D, S, S1) :- D #< MaxDepth, take_step(DFN, S, S1).

%% is_space(+DFN, +X/Y) is semidet
%  True if X/Y is a space in the maze defined by the given DFN (designer's
%  favourite number).
is_space(DFN, X/Y) :-
  N #= X*X + 3*X + 2*X*Y + Y + Y*Y + DFN,
  int_bin(N, B),
  count(#=(1), B, OneCount), !,
  OneCount mod 2 #= 0.

%% int_bin(?N, ?B) is det
%  When N is an integer >= 0, B is a list of the digits in its binary
%  representation.
int_bin(N, B) :- var(B), int_bin(N, B, []).
int_bin(N, B) :- var(N), foldl([A, B, R]>>(R #= B << 1 \/ A), B, 0, N).

int_bin(0, B, B) :- !.
int_bin(N, B, Acc) :-
  N #> 0, LSB #= N /\ 1, N1 #= N >> 1, int_bin(N1, B, [LSB|Acc]).
