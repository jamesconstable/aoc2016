#! /usr/bin/env swipl

% Solver for Day 21 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/21

:- initialization(main, main).
:- ensure_loaded('common.pl').

:- use_module(library('dcg/high_order')).

main(['1']) :- !, read_input(input_dcg(I)), writeln(I).
main(['2']) :- !, read_input(input_dcg(I)), part2(I, R), writeln(R).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

% Input grammar.
input_dcg([C|Cs]) --> command_dcg(C), blanks, input_dcg(Cs).
input_dcg([]) --> blanks.

command_dcg(swap_i(A, B)) -->
  "swap position ", integer(A), " with position ", integer(B).
command_dcg(swap_c(A, B)) -->
  "swap letter ", nonblank(AC), " with letter ", nonblank(BC),
  { char_code(A, AC), char_code(B, BC) }.
command_dcg(rotate_i(I)) -->
  "rotate left ", integer(I), " step", optional("s", { true }).
command_dcg(rotate_i(-I)) -->
  "rotate right ", integer(I), " step", optional("s", { true }).
command_dcg(rotate_c(C)) -->
  "rotate based on position of letter ", nonblank(CC), { char_code(C, CC) }.
command_dcg(reverse(I, J)) -->
  "reverse positions ", integer(I), " through ", integer(J).
command_dcg(move_i(I, J)) -->
  "move position ", integer(I), " to position ", integer(J).
