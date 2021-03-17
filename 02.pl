#! /usr/bin/env swipl

% Solver for Day 2 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/2

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- read_input(instructions(Is)), part1(Is, R), writeln(R).
main(['2']) :- writeln("TODO Part 2").
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(Is, R) :-
  scanl(follow_instruction, Is, 1/1, [_|Dests]),
  maplist(keypad, Dests, Digits),
  atomic_list_concat(Digits, R).

% Part 1 keypad layout.
keypad(0/0, 1).  keypad(1/0, 2).  keypad(2/0, 3).
keypad(0/1, 4).  keypad(1/1, 5).  keypad(2/1, 6).
keypad(0/2, 7).  keypad(1/2, 8).  keypad(2/2, 9).

follow_instruction(Ds, Start, End) :- foldl(move, Ds, Start, End).

%% move(?Direction, ?Start, ?End) is multi
%  Coordinate End is the result of moving in Direction from coordinate Start.
%  Attempting to move off the keypad has no effect.
move(up,    X/Y, X/Y1) :- Y1 #= Y-1, valid_move(X/Y, X/Y1).
move(up,    X/Y, X/Y)  :- Y1 #= Y-1, valid_edge(X/Y, X/Y1).
move(down,  X/Y, X/Y1) :- Y1 #= Y+1, valid_move(X/Y, X/Y1).
move(down,  X/Y, X/Y)  :- Y1 #= Y+1, valid_edge(X/Y, X/Y1).
move(left,  X/Y, X1/Y) :- X1 #= X-1, valid_move(X/Y, X1/Y).
move(left,  X/Y, X/Y)  :- X1 #= X-1, valid_edge(X/Y, X1/Y).
move(right, X/Y, X1/Y) :- X1 #= X+1, valid_move(X/Y, X1/Y).
move(right, X/Y, X/Y)  :- X1 #= X+1, valid_edge(X/Y, X1/Y).

valid_move(Before, After) :- keypad(Before, _), keypad(After, _).
valid_edge(Before, After) :- keypad(Before, _), \+ keypad(After, _).

% Input grammar
instructions([I|Is]) --> instruction_line(I), instructions(Is).
instructions([])     --> remainder(_).
instruction_line([D|Ds]) --> direction(D), instruction_line(Ds).
instruction_line([])     --> "\n".
direction(up)    --> "U".
direction(down)  --> "D".
direction(left)  --> "L".
direction(right) --> "R".
