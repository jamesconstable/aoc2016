#! /usr/bin/env swipl

% Solver for Day 24 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/24

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, read_input(map_dcg(Ls)), part1(Ls, C), writeln(C).
main(['2']) :- !, part2(R), writeln(R).
main(_) :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(Map, MinSteps) :-
  points_of_interest(Ps, Map),
  memberchk('0'-X0/Y0, Ps),
  findall(
    P-State-Dist,
    (member(P-SX/SY, Ps),
      bfs(s(SX, SY, Map), expand_fringe, is_goal, state_coord, State, Dist),
      writeln(P-Dist)),
    PSDs),
  maplist(
    [Start-s(X, Y, Map)-D, Start-End-D]>>map_get(X, Y, Map, End),
    PSDs, PairDistsUnsorted),
  sort(PairDistsUnsorted, PairDists),
  %writeln(PairDists),
  maplist([Digit-_, Digit]>>true, Ps, POIs),
  %writeln(POIs),
  findall(
    Sum,
    (permutation(POIs, ['0'|Route]),
      %write(Route), tab(5),
      append(Route, ['0'], Route1),
      pairwise(['0'|Route1], RoutePairs),
      %write(RoutePairs), tab(5),
      maplist(
        {PairDists}/[SS-ES, SD]>>memberchk(SS-ES-SD, PairDists), RoutePairs, RouteDists),
      %writeln(RouteDists),
      sum_list(RouteDists, Sum)),
    Sums),
  min_list(Sums, MinSteps).

pairwise([], []).
pairwise([_], []).
pairwise([X, Y|T], [X-Y|R]) :- pairwise([Y|T], R).

points_of_interest(Ps, Map) :-
  findall(
    V-X/Y,
    (max_indices(Map, XMax, YMax),
      between(0, XMax, X),
      between(0, YMax, Y),
      map_get(X, Y, Map, V),
      char_type(V, digit)),
    Ps).

max_indices(Map, X, Y) :-
  [Row0|_] = Map,
  length(Map, Rows), Y #= Rows - 1,
  length(Row0, Cols), X #= Cols - 1.

expand_fringe(_, s(X, Y, Map), s(X1, Y, Map)) :-
  X1 #= X - 1, map_get(X1, Y, Map, V), V \= '#'.
expand_fringe(_, s(X, Y, Map), s(X1, Y, Map)) :-
  X1 #= X + 1, map_get(X1, Y, Map, V), V \= '#'.
expand_fringe(_, s(X, Y, Map), s(X, Y1, Map)) :-
  Y1 #= Y - 1, map_get(X, Y1, Map, V), V \= '#'.
expand_fringe(_, s(X, Y, Map), s(X, Y1, Map)) :-
  Y1 #= Y + 1, map_get(X, Y1, Map, V), V \= '#'.

is_goal(s(X, Y, Map)) :- map_get(X, Y, Map, V), char_type(V, digit).

state_coord(s(X, Y, _), X-Y).

map_get(X, Y, Map, V) :- nth0(Y, Map, Row), nth0(X, Row, V).

% Input grammar
map_dcg([Chars|Rest]) -->
  nonblanks(Codes),
  { length(Codes, L), L #> 0, maplist(char_code, Chars, Codes) },
  blanks,
  map_dcg(Rest).
map_dcg([]) --> blanks.
