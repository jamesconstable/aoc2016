#! /usr/bin/env swipl

% Solver for Day 1 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/1

:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).

:- initialization(main, main).

main(['1']) :-
  read_input(Instructions),
  foldl(follow_instruction, Instructions, s(0/0, 0/1), s(Dest, _)),
  manhatten_distance(0/0, Dest, D),
  writeln(D).
main(['2']) :- writeln("Part 2").

follow_instruction(Turn-Distance, s(Start, StartDir), s(End, EndDir)) :-
  rotate(Turn, StartDir, EndDir),
  move(Start, EndDir, Distance, End).

rotate(left, X/Y, (-Y)/X).
rotate(right, X/Y, Y/(-X)).

move(X/Y, XMult/YMult, D, (X + D*XMult)/(Y + D*YMult)).

manhatten_distance(X1/Y1, X2/Y2, D) :- D #= abs(X1-X2) + abs(Y1-Y2).

% Input grammar
step([left-D|T]) --> "L", integer(D), step(T).
step([right-D|T]) --> "R", integer(D), step(T).
step(T) --> ", ", step(T).
step([]) --> remainder(_).

read_input(Instructions) :- phrase_from_stream(step(Instructions), user_input).
