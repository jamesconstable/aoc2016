#! /usr/bin/env swipl

% Solver for Day 22 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/22

:- initialization(main, main).
:- ensure_loaded('common.pl').

:- use_module(library('dcg/high_order')).

main(['1']) :- !, read_input(input_dcg(Ns)), part1(Ns, R), writeln(R).
main(['2']) :- !, read_input(input_dcg(Ns)), writeln(Ns).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(Nodes, Count) :-
  findall(A-B, (member(A, Nodes), member(B, Nodes), viable(A, B)), Pairs),
  length(Pairs, Count).

viable(node(A, _, Used, _, _), node(B, _, _, Free, _)) :-
  A \= B,
  Used #> 0,
  Free #>= Used.

% Input grammar.
input_dcg(Ns) -->
  string_without("\n", _), "\n",
  string_without("\n", _), "\n",
  nodes_dcg(Ns).

nodes_dcg([N|Ns]) --> node_dcg(N), blanks, nodes_dcg(Ns).
nodes_dcg([]) --> blanks.

node_dcg(node(Name, Size, Used, Free, Percent)) -->
  nonblanks(NameCodes), blanks, { atom_codes(Name, NameCodes) },
  integer(Size), "T", blanks,
  integer(Used), "T", blanks,
  integer(Free), "T", blanks,
  integer(Percent), "%", blanks.
