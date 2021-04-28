#! /usr/bin/env swipl

% Solver for Day 21 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/21

:- initialization(main, main).
:- ensure_loaded('common.pl').

:- use_module(library('dcg/high_order')).

main(['1']) :- !, read_input(input_dcg(Cs)), part1(Cs, R), writeln(R).
main(['2']) :- !, read_input(input_dcg(I)), part2(I, R), writeln(R).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(Cs, R) :- atom_chars(abcdefgh, P), scrambled(P, Cs, S), atom_chars(R, S).

scrambled(P, Cs, S) :- scramble_loop(P, Cs, S).

scramble_loop(P, [], P).
scramble_loop(P, [C|Cs], S) :-
  once(run_command(C, P, R)), scramble_loop(R, Cs, S).

run_command(swap_i(I, J), P, R) :-
  nth0(I, P, A), nth0(J, P, B), set_nth0(I, P, B, P1), set_nth0(J, P1, A, R).
run_command(swap_c(A, B), P, R) :-
  nth0(I, P, A), nth0(J, P, B), set_nth0(I, P, B, P1), set_nth0(J, P1, A, R).
run_command(rotate_i(I), P, R) :- rotate_list(I, P, R).
run_command(rotate_c(C), P, R) :-
  nth0(I, P, C),
  (I #>= 4 -> FourFactor = 1; FourFactor = 0),
  N #= -I - 1 - FourFactor,
  rotate_list(N, P, R).
run_command(reverse(I, J), P, R) :-
  K #= J - I + 1,
  split_at(I, P, Before, T),
  split_at(K, T, Mid, After),
  reverse(Mid, MidRev),
  flatten([Before, MidRev, After], R).
run_command(move_i(I, J), P, R) :- nth0(I, P, C, P1), nth0(J, R, C, P1).

% Input grammar.
input_dcg([C|Cs]) --> command_dcg(C), blanks, input_dcg(Cs).
input_dcg([]) --> blanks.

command_dcg(swap_i(I, J)) -->
  "swap position ", integer(I), " with position ", integer(J).
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
