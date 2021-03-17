#! /usr/bin/env swipl

% Solver for Day 1 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/1

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- read_input(steps(Steps)), part1(Steps, R), writeln(R).
main(['2']) :- read_input(steps(Steps)), part2(Steps, R), writeln(R).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

% The state functor has the structure state(X/Y, DX/DY, Seen), where X/Y is the
% current location, DX/DY is a unit vector indicating the current direction
% (e.g. North is 0/1), and Seen is an assoc whose keys are the previously
% visited locations (ignored for part 1). The dest functor used in part 2 has
% the same structure but is used to indicate when a destination has been found.

part1(Steps, Distance) :-
  foldl(follow_step, Steps, state(0/0, 0/1, _), state(Dest, _, _)),
  manhatten_distance(0/0, Dest, Distance).

part2(Steps, Distance) :-
  list_to_assoc([0/0-_], Seen),
  foldl(follow_to_seen, Steps, state(0/0, 0/1, Seen), dest(Dest, _, _)),
  manhatten_distance(0/0, Dest, Distance).

%% follow_step(+Step, +Start, -End) is det
%  End is Start after executing Step.
follow_step(Turn-Distance, Start, End) :-
  rotate(Turn, Start, Rotated),
  move(Distance, Rotated, End).

%% follow_to_seen(+Step, +Start, -End) is multi
%  End is Start after executing Step, unless this causes it to revisit a
%  previous location, in which case movement is stopped at that point and End
%  uses the dest functor. Ignores first destination on backtracking and
%  continues until subsequent visited locations are found.
follow_to_seen(_, dest(L, D, S), dest(L, D, S)).
follow_to_seen(Turn-Distance, Start, End) :-
  rotate(Turn, Start, Rotated),
  visit_move(Distance, Rotated, End).

%% rotate(?Direction, ?State, ?State1) is det
%  State1 is State, but with its movement vector rotated 90° in Direction.
rotate(left, state(L, X/Y, S), state(L, (-Y)/X, S)).
rotate(right, state(L, X/Y, S), state(L, Y/(-X), S)).

%% move(?Distance, ?State, ?State1) is det
%  State1 is State moved Distance units in the current direction.
move(D, state(X/Y, DX/DY, _), state((X + D*DX)/(Y + D*DY), DX/DY, _)).

%% visit_move(+Distance, +State, -State1) is multi
%  State1 is State moved Distance units in the current direction, unless the
%  move passes through a previously visited location, in which case movement is
%  stopped at that point and the dest functor is used. Ignores first destination
%  on backtracking and continues until subsequent visited locations are found.
visit_move(0, State, State).
visit_move(_, dest(L, D, S), dest(L, D, S)).
visit_move(D, state(X/Y, DX/DY, Seen), dest(X1/Y1, DX/DY, Seen)) :-
  D > 0,
  X1 is X + DX, Y1 is Y + DY,
  get_assoc(X1/Y1, Seen, _).
visit_move(D, state(X/Y, DX/DY, Seen), R) :-
  D > 0, D1 is D-1,
  X1 is X + DX, Y1 is Y + DY,
  put_assoc(X1/Y1, Seen, _, Seen1),
  visit_move(D1, state(X1/Y1, DX/DY, Seen1), R).

%% manhatten_distance(?Start, ?End, ?Distance) is det
%  Distance is the Manhatten distance between Start and End. In the (?, ?, +)
%  flow order, CLP enumeration can be used to find all points separated by that
%  distance.
manhatten_distance(X1/Y1, X2/Y2, D) :- D #= abs(X1-X2) + abs(Y1-Y2).

% Input grammar
steps([left-D|T]) --> "L", integer(D), steps(T).
steps([right-D|T]) --> "R", integer(D), steps(T).
steps(T) --> ", ", steps(T).
steps([]) --> remainder(_).
