#! /usr/bin/env swipl

% Solver for Day 22 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/22

% Note that Part 2 isn't really a solver so much as a pretty printer. A
% general solution would be prohibitely slow, but from studying the values in
% the data, it's clear that this isn't really necessary. The problem in this
% case simplifies to a sliding tile puzzle, which when pretty printed, can be
% solved quickly enough by hand (it is frustrating though).

% I considered writing an A* solution for it taking into account the structure
% of the data, but I've done enough of those already this AoC, and besides, the
% problem author apparently intended for the solution to be found manually:
% https://www.reddit.com/r/adventofcode/comments/5jor9q/2016_day_22_solutions/dbhwg4l?utm_source=share&utm_medium=web2x&context=3

:- initialization(main, main).
:- ensure_loaded('common.pl').

:- use_module(library('dcg/high_order')).

main(['1']) :- !, read_input(input_dcg(Ns)), part1(Ns, R), writeln(R).
main(['2']) :- !, read_input(input_dcg(Ns)), part2(Ns).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(Nodes, Count) :-
  findall(A-B, (member(A, Nodes), member(B, Nodes), viable(A, B)), Pairs),
  length(Pairs, Count).

part2(Nodes) :- write_grid(Nodes).

%% viable(+Node1, +Node2) is semidet
%  True if Node1 and Node2 form a valid transfer pair, according to the
%  specification given in Part 1.
viable(node(A, _, Used, _, _), node(B, _, _, Free, _)) :-
  A \= B,
  Used #> 0,
  Free #>= Used.

%% write_grid(+Nodes)
%  Writes the grid to stdout in a similar format to that used in the example.
write_grid(Nodes) :-
  max_member(node(MaxX/MaxY, _, _, _, _), Nodes),
  between(0, MaxY, Y),
  nl,
  between(0, MaxX, X),
  memberchk(node(X/Y, _, Used, _, _), Nodes),
  write_cell(X/Y, MaxX/MaxY, Used),
  fail.
write_grid(_) :- nl.

write_cell(0/0, _, _) :- write('S'), !.
write_cell(MaxX/0, MaxX/_, _) :- write('G'), !.
write_cell(_, _, 0) :- write('_'), !.
write_cell(_, _, V) :- V #< 100, write('.'), !.
write_cell(_, _, V) :- V #>= 100, write('#').

% Input grammar.
input_dcg(Ns) -->
  string_without("\n", _), "\n",
  string_without("\n", _), "\n",
  nodes_dcg(Ns).

nodes_dcg([N|Ns]) --> node_dcg(N), blanks, nodes_dcg(Ns).
nodes_dcg([]) --> blanks.

node_dcg(node(X/Y, Size, Used, Free, Percent)) -->
  "/dev/grid/node-x", integer(X), "-y", integer(Y), blanks,
  integer(Size), "T", blanks,
  integer(Used), "T", blanks,
  integer(Free), "T", blanks,
  integer(Percent), "%", blanks.
