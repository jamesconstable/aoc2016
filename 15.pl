#! /usr/bin/env swipl

% Solver for Day 15 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/15

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, read_input(discs_dcg(Ds)), part1(Ds, T), writeln(T).
main(['2']) :- !, read_input(discs_dcg(Ds)), part2(Ds, T), writeln(T).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(Discs, T) :- drop_time(Discs, T).

part2(Discs, T) :- append(Discs, [disc(11, 0)], Discs1), drop_time(Discs1, T).

%% drop_time(+Discs, -Time) is det
%  Time is the earliest time that the button can be pressed such that the
%  capsule will make it through all Discs. Each disc is a functor of the form
%  disc(NumPositions, HoleAtTime0).
drop_time(Discs, Time) :-
  length(Discs, N),
  numlist(1, N, Offsets),
  maplist([disc(D, R), O, R1-D]>>(R1 #= D - R - O), Discs, Offsets, RMs),
  chinese_remainder(RMs, Time).

%% chinese_remainder(+RMs, -X) is det
%  Given a list of remainder-divisor pairs (RDs), X is the minimum positive
%  number such that X mod D = R for every pair.
chinese_remainder(RDs, X) :- foldl(chinese_remainder, RDs, 0-1, X-_).

chinese_remainder(R1-D1, R2-D2, R-D) :-
  modular_inverse(D2, D1, DInv),
  D #= D2 * D1,
  R #= (R2 + D2 * (R1 - R2) * DInv) mod D.

%% modular_inverse(+X, +M, -Inv) is det
%  Inv is the modular multiplicative inverse of X under modulo M.
%  I.e. X * Inv ≅ 1 (mod M).
modular_inverse(X, M, Inv) :-
  gcd_extended(X, M, _, I, _),
  Inv #= I mod M.

%% gcd_extended(+A, +B, -GCD, -X, -Y) is det
%  GCD is the greatest common divisor of A and B, with X and Y such that
%  A*X + B*Y = GCD. Uses the extended Euclidean algorithm.
%  Time complexity: O(log(min(A, B)))
gcd_extended(0, B, B, 0, 1) :- !.
gcd_extended(A, B, GCD, X, X1) :-
  A #\= 0,
  BModA #= B mod A,
  gcd_extended(BModA, A, GCD, X1, Y1),
  X #= Y1 - B // A * X1.

% Input grammar
disc_dcg(disc(Positions, Start)) -->
  "Disc #", integer(_), " has ", integer(Positions),
  " positions; at time=0, it is at position ", integer(Start), ".".

discs_dcg([D|Ds]) --> disc_dcg(D), blanks, discs_dcg(Ds).
discs_dcg([])     --> [].
