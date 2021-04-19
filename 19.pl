#! /usr/bin/env swipl

% Solver for Day 19 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/19

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, read_input(input_dcg(I)), part1(I, R), writeln(R).
main(['2']) :- !, read_input(input_dcg(I)), part2(I, R), writeln(R).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(N, Winner) :-
  numlist(1, N, Elves),
  white_elephant_loop(elf_turn, s(Elves, L, L), Winner).

part2(N, Winner) :-
  numlist(1, N, Elves),
  Split #= N // 2 - 1 + N mod 2,
  dlist_split_at(Split, Elves, Start, Start1, End),
  white_elephant_loop(elf_turn2, s(N, End, Start, Start1), Winner).

white_elephant_loop(_, t(Winner), Winner) :- !.
white_elephant_loop(Pred, S, Winner) :-
  call(Pred, S, S1), !,
  white_elephant_loop(Pred, S1, Winner).

%% elf_turn(S, S1) is semidet
%  Given a game state S, S1 is the state resulting from a single elf's turn.
%  In-progress states are functors of the form s(T, L, L1), where T is the tail
%  of a list representing the game circle in sitting order (starting just after
%  the last elf to have been removed), and L/L1 is a difference list
%  representing the start of the circle. Elves move from T to L/L1 as their
%  turn occurs. Completed states are represented by a functor of the form t(W),
%  where W is the number of the winning elf.
elf_turn(s([E], L, L), t(E)) :- var(L), !.
elf_turn(s([], L, []), R) :- elf_turn(s(L, N, N), R).
elf_turn(s([E], [_|R], [E]), s(R, L, L)).
elf_turn(s([E, _|R], L, [E|L1]), s(R, L, L1)).

%% dlist_split_at(N, L, Start, StartH, End) is det
%  Start/StartH is a difference list containing the first N elements of L, with
%  End containing the remaining elements. Fails if N is greater than the length
%  of L.
dlist_split_at(0, L, S, S, L).
dlist_split_at(N, [X|Xs], Start, StartH, End) :-
  N #> 0,
  N1 #= N-1,
  Start = [X|Start1],
  dlist_split_at(N1, Xs, Start1, StartH, End).

%% elf_turn2(S, S1) is semidet
%  Given a game state S, S1 is the state resulting from a single elf's turn,
%  according to the new rules used in Part 2. In-progress states are functors of
%  the form s(N, T, L, L1), where N is the number of elves remaining, T is the
%  tail of a list representing the game circle in sitting order (starting just
%  after the last elf to have been removed), and L/L1 is a difference list
%  representing the start of the circle. Elves move from T to L/L1 as the danger
%  of them being eliminated passes (i.e. the head of T is always the opposite
%  point of the circle to the elf whose turn it is). Completed states are
%  represented by a functor of the form t(W), where W is the number of the
%  winning elf.
elf_turn2(s(N, [], L, []), R) :- elf_turn2(s(N, L, E, E), R).
elf_turn2(s(1, [E], _, _), t(E)).
elf_turn2(s(N, [E, _|R], L, [E|L1]), s(N1, R, L, L1)) :-
  N #> 1, N mod 2 #= 0,
  N1 #= N-1.
elf_turn2(s(N, [E], [_|L], [E]), s(N1, L, L1, L1)) :-
  N #> 1, N mod 2 #= 0,
  N1 #= N-1.
elf_turn2(s(N, [_|R], L, L1), s(N1, R, L, L1)) :-
  N #> 1, N mod 2 #= 1,
  N1 #= N-1.

% Input grammar.
input_dcg(C) --> integer(C), blanks.
