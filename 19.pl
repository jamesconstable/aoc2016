#! /usr/bin/env swipl

% Solver for Day 19 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/19

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, read_input(input_dcg(I)), part1(I, R), writeln(R).
main(['2']) :- !.%, read_input(input_dcg(I)), part2(I, R), writeln(R).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(I, C) :- safe_count_rows(I, 40, C).

white_elephant(N, Winner) :-
  numlist(1, N, Elves),
  white_elephant_loop(s(Elves, L, L), Winner).

white_elephant_loop(t(Winner), Winner).
white_elephant_loop(s(L1, L2, L3), Winner) :-
  elf_turn(s(L1, L2, L3), S),
  white_elephant_loop(S, Winner).

elf_turn(s([E], L, L), t(E)) :- var(L).
elf_turn(s([], L, []), R) :- elf_turn(s(L, N, N), R).
elf_turn(s([E], [_|R], [E]), s(R, L, L)).
elf_turn(s([E, _|R], L, [E|L1]), s(R, L, L1)).

% Input grammar.
input_dcg(C) --> integer(C), blanks.
