#! /usr/bin/env swipl

% Solver for Day 2 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/2

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, read_input(instructions(I)), solve(pad1, I, 1/1, R), writeln(R).
main(['2']) :- !, read_input(instructions(I)), solve(pad2, I, 0/2, R), writeln(R).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

solve(Pad, Is, Start, R) :-
  scanl(follow_instruction(Pad), Is, Start, [_|Dests]),
  maplist(Pad, Dests, Digits),
  atomic_list_concat(Digits, R).

follow_instruction(Pad, Ds, Start, End) :- foldl(move(Pad), Ds, Start, End).

% Part 1 keypad layout.
pad1(0/0, 1).  pad1(1/0, 2).  pad1(2/0, 3).
pad1(0/1, 4).  pad1(1/1, 5).  pad1(2/1, 6).
pad1(0/2, 7).  pad1(1/2, 8).  pad1(2/2, 9).

% Part 2 keypad layout.
                                pad2(2/0, 1).
               pad2(1/1, 2).    pad2(2/1, 3).    pad2(3/1, 4).
pad2(0/2, 5).  pad2(1/2, 6).    pad2(2/2, 7).    pad2(3/2, 8).    pad2(4/2, 9).
               pad2(1/3, 'A').  pad2(2/3, 'B').  pad2(3/3, 'C').
                                pad2(2/4, 'D').

%% move(:Pad, ?Direction, ?Start, ?End) is multi
%  Coordinate End is the result of moving in Direction from coordinate Start,
%  on the keypad represented by Pad. Attempting to move off the keypad has no
%  effect.
move(Pad, up,    X/Y, X/Y1) :- Y1 #= Y-1, valid_move(Pad, X/Y, X/Y1).
move(Pad, up,    X/Y, X/Y)  :- Y1 #= Y-1, valid_edge(Pad, X/Y, X/Y1).
move(Pad, down,  X/Y, X/Y1) :- Y1 #= Y+1, valid_move(Pad, X/Y, X/Y1).
move(Pad, down,  X/Y, X/Y)  :- Y1 #= Y+1, valid_edge(Pad, X/Y, X/Y1).
move(Pad, left,  X/Y, X1/Y) :- X1 #= X-1, valid_move(Pad, X/Y, X1/Y).
move(Pad, left,  X/Y, X/Y)  :- X1 #= X-1, valid_edge(Pad, X/Y, X1/Y).
move(Pad, right, X/Y, X1/Y) :- X1 #= X+1, valid_move(Pad, X/Y, X1/Y).
move(Pad, right, X/Y, X/Y)  :- X1 #= X+1, valid_edge(Pad, X/Y, X1/Y).

valid_move(Pad, Before, After) :- call(Pad, Before, _), call(Pad, After, _).
valid_edge(Pad, Before, After) :- call(Pad, Before, _), \+ call(Pad, After, _).

% Input grammar
instructions([I|Is]) --> instruction_line(I), instructions(Is).
instructions([])     --> remainder(_).
instruction_line([D|Ds]) --> direction(D), instruction_line(Ds).
instruction_line([])     --> "\n".
direction(up)    --> "U".
direction(down)  --> "D".
direction(left)  --> "L".
direction(right) --> "R".
