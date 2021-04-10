#! /usr/bin/env swipl

% Solver for Day 18 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/18

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, read_input(input_dcg(I)), part1(I, R), writeln(R).
main(['2']) :- !, read_input(input_dcg(I)), part2(I, R), writeln(R).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(I, SafeCount) :-
  iterate(40, line_succ, I, Lines),
  maplist(count(=('.')), Lines, Counts),
  sum_list(Counts, SafeCount).

part2(I, SafeCount) :-
  iterate(400000, line_succ, I, Lines),
  maplist(count(=('.')), Lines, Counts),
  sum_list(Counts, SafeCount).

iterate(0, _, _, []).
iterate(N, Goal, I, [I|R]) :-
  N #> 0, N1 #= N-1,
  call(Goal, I, I2),
  iterate(N1, Goal, I2, R).

line_succ(L, S) :- append(['.'|L], ['.'], WithWalls), line_succ_(WithWalls, S).

line_succ_(['^', '^', '.'|T], ['^'|R]) :- line_succ_(['^', '.'|T], R).
line_succ_(['.', '^', '^'|T], ['^'|R]) :- line_succ_(['^', '^'|T], R).
line_succ_(['^', '.', '.'|T], ['^'|R]) :- line_succ_(['.', '.'|T], R).
line_succ_(['.', '.', '^'|T], ['^'|R]) :- line_succ_(['.', '^'|T], R).
line_succ_([_, A, B|T],       ['.'|R]) :- line_succ_([A, B|T], R).
line_succ_([_, _],            []).

% Input grammar.
input_dcg(S) --> nonblanks(C), blanks, { maplist(char_code, S, C) }.
