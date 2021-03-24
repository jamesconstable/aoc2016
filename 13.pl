#! /usr/bin/env swipl

% Solver for Day 13 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/13

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- read_input((integer(DFN), blanks)), part1(DFN, R), writeln(R).
main(['2']) :- read_input((integer(DFN), blanks)), part2(DFN, R), writeln(R).
main(_) :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(DFN, R) :- bfs(1/1, unbounded_step(DFN), =(31/39), (=), _, R).

part2(DFN, R) :-
  findall(C, bfs(1/1, bounded_step(DFN, 50), yes, (=), _, C), Cs),
  length(Cs, R).

%% bfs(+Init, :NextState, :IsGoal, :Canonical, -Final, -Cost)
%  Performs a breadth-first search given the following parameters:
%    * Init - the initial state
%    * NextState - a predicate that generates successor states on backtracking
%    * IsGoal - a predicate that takes a state and checks whether it is a valid
%        goal state
%    * Canonical - a predicate that converts a state to a canonical
%        representation (e.g. a hash function), for use in avoiding previously
%        visited states. Useful for pruning the search space if the problem has
%        many equivalent but non-identical states. If not required, (=) can be
%        used as an identity predicate.
%    * Final - the first goal state encountered. Returns successive, higher cost
%        goals on backtracking.
%    * Cost - the number of steps taken to reach Final
%  Fails if search space is fully expanded without encountering a goal state.
bfs(Init, NextState, IsGoal, Canonical, Final, Cost) :-
  empty_assoc(Seen),
  bfs([0-Init|H], H, Seen, NextState, IsGoal, Canonical, Final, Cost).

bfs(Q, _, _, _, _, _, _, _) :-
  var(Q), !, fail.
bfs([Step-State|_], _, Seen, _, IsGoal, Canonical, State, Step) :-
  call(IsGoal, State),
  \+ is_seen(State, Canonical, Seen).
bfs([Step-State|Q], Q1, Seen, NextState, IsGoal, Canonical, Final, Cost) :-
  call(Canonical, State, StateC),
  put_assoc(StateC, Seen, _, Seen1),
  findall(Next, call(NextState, Step, State, Next), NextStates),
  Step1 #= Step + 1,
  maplist({Step1}/[S, Step1-S]>>true, NextStates, NextStates1),
  exclude(
    {Canonical, Seen1}/[Cost-S]>>is_seen(S, Canonical, Seen1),
    NextStates1, NextStates2),
  append(NextStates2, Q2, Q1),
  bfs(Q, Q2, Seen1, NextState, IsGoal, Canonical, Final, Cost).

%% is_seen(+State, :Canonical, +Seen) is semidet
%  Succeeds if the canonical form of State is in Seen.
is_seen(State, Canonical, Seen) :-
  call(Canonical, State, StateC),
  get_assoc(StateC, Seen, _).

%% take_step(+DFN, ?X/Y, ?X1/Y1) is nondet
%  X1/Y1 can be reached in a single step from X/Y in the maze defined by the
%  given DFN (designer's favourite number).
take_step(DFN, XY, XY1) :-
  take_step(XY, XY1), is_space(DFN, XY), is_space(DFN, XY1).

take_step(X/Y, X1/Y) :- X #> 0, X1 #= X - 1.
take_step(X/Y, X1/Y) :- X1 #= X + 1.
take_step(X/Y, X/Y1) :- Y #> 0, Y1 #= Y - 1.
take_step(X/Y, X/Y1) :- Y1 #= Y + 1.

unbounded_step(DFN, _, S, S1) :- take_step(DFN, S, S1).
bounded_step(DFN, MaxDepth, D, S, S1) :- D #< MaxDepth, take_step(DFN, S, S1).

%% is_space(+DFN, +X/Y) is semidet
%  True if X/Y is a space in the maze defined by the given DFN (designer's
%  favourite number).
is_space(DFN, X/Y) :-
  N #= X*X + 3*X + 2*X*Y + Y + Y*Y + DFN,
  int_bin(N, B),
  count(#=(1), B, OneCount), !,
  OneCount mod 2 #= 0.

%% int_bin(?N, ?B) is det
%  When N is an integer >= 0, B is a list of the digits in its binary
%  representation.
int_bin(N, B) :- var(B), int_bin(N, B, []).
int_bin(N, B) :- var(N), foldl([A, B, R]>>(R #= B << 1 \/ A), B, 0, N).

int_bin(0, B, B) :- !.
int_bin(N, B, Acc) :-
  N #> 0, LSB #= N /\ 1, N1 #= N >> 1, int_bin(N1, B, [LSB|Acc]).
