#! /usr/bin/env swipl

% Solver for Day 20 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/20

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, read_input(input_dcg(I)), part1(I, R), writeln(R).
main(['2']) :- !, read_input(input_dcg(I)), part2(I, R), writeln(R).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(I, R) :- msort(I, S), once(valid_ip(S, R)).

valid_ip(Ranges, Result) :- valid_ip(0, Ranges, Result).

valid_ip(N, _, _) :- N #> 4294967295, !, fail.
valid_ip(N, [], N).
valid_ip(N, [_-U|Ps], R) :- U #< N, !, valid_ip(N, Ps, R).
valid_ip(N, [L-U|Ps], R) :- L #=< N, N1 #= U+1, !, valid_ip(N1, Ps, R).
valid_ip(N, [L-_|_], N) :- L #> N.
valid_ip(N, Ps, R) :- N1 #= N+1, valid_ip(N1, Ps, R).

% Input grammar.
input_dcg([L-U|Ls]) --> line_dcg(L, U), blanks, input_dcg(Ls).
input_dcg([]) --> blanks.

line_dcg(Lower, Upper) --> integer(Lower), "-", integer(Upper).
