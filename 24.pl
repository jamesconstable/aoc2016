#! /usr/bin/env swipl

% Solver for Day 24 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/24

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, read_input(map_dcg(M)), min_route(M, [], _, S), writeln(S).
main(['2']) :- !, read_input(map_dcg(M)), min_route(M, ['0'], _, S), writeln(S).
main(_) :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

%% min_route(+Map, +ExtraStops, -Route, -Steps)
%  Route is the minimum length path (of length Steps) around all points of
%  interest in Map, starting at 0, and including any ExtraStops (a list of POIs
%  to be revisited) at the end.
min_route(Map, ExtraStops, Route, Steps) :-
  points_of_interest(Ps, Map),
  pair_distances(Ps, Map, PairDists),
  maplist([Digit-_, Digit]>>true, Ps, POIs),
  findall(
    Length,
    (permutation(POIs, ['0'|Route]),
      append(Route, ExtraStops, Route1),
      route_length(['0'|Route1], PairDists, Length)),
    Lengths),
  min_list(Lengths, Steps).

%% points_of_interest(-POIs, +Map) is det
%  POIs is a list containing the points of interest (number-coordinate pairs) in
%  Map.
points_of_interest(POIs, Map) :-
  findall(
    V-X/Y,
    (max_indices(Map, XMax, YMax),
      between(0, XMax, X),
      between(0, YMax, Y),
      map_get(X/Y, Map, V),
      char_type(V, digit)),
    POIs).

%% max_indices(+Map, -X, -Y) is det
%  X and Y are the maximum X and Y coordinates (zero-based) respectively in Map.
max_indices(Map, X, Y) :-
  [Row0|_] = Map,
  length(Map, Rows), Y #= Rows - 1,
  length(Row0, Cols), X #= Cols - 1.

%% pair_distances(+Ps, +Map, -PairDists) is det
%  PairDists is a list of functors of the form Start-End-Distance, representing
%  the distance between each pair of points-of-interest (Ps) in Map.
pair_distances(POIs, Map, PairDists) :-
  findall(
    Start-End-Distance,
    (member(Start-P, POIs),
      pair_distance(Start-P, End-_, Map, Distance)),
    PairDistsUnsorted),
  sort(PairDistsUnsorted, PairDists).

%% pair_distance(+POI1, -POI2, +Map, -Dist) is multi
%  Dist is the number of steps between POI1 and POI2 in Map. POI1 must be
%  provided, while POI2 takes the value of successively further points of
%  interest on backtracking.
pair_distance(_-P1, Name2-P2, Map, Dist) :-
  bfs(s(P1, Map), step, is_goal, [s(P, _), P]>>true, s(P2, Map), Dist),
  map_get(P2, Map, Name2).

%% step(_, +State0, -State1) is multi
%  State1 is reachable in one step from State0. The first argument is not used,
%  but needed to match the signature required by bfs/6.
step(_, s(X/Y, M), s(X1/Y, M)) :- X1 #= X - 1, \+ wall(X1/Y, M).
step(_, s(X/Y, M), s(X1/Y, M)) :- X1 #= X + 1, \+ wall(X1/Y, M).
step(_, s(X/Y, M), s(X/Y1, M)) :- Y1 #= Y - 1, \+ wall(X/Y1, M).
step(_, s(X/Y, M), s(X/Y1, M)) :- Y1 #= Y + 1, \+ wall(X/Y1, M).

%% wall(+Coord, +Map) is semidet
%  Succeeds if Coord points to a wall in Map.
wall(Coord, Map) :- map_get(Coord, Map, '#').

%% is_goal(+State) is semidet
%  Succeeds if State is a point of interest.
is_goal(s(P, Map)) :- map_get(P, Map, V), char_type(V, digit).

%% map_get(+Coord, +Map, -Value) is det
%  Map (a 2D list) contains Value at Coord (a functor of the form X/Y).
map_get(X/Y, Map, Value) :- nth0(Y, Map, Row), nth0(X, Row, Value).

%% route_length(+Route, +PairDists, -Length) is det
%  Route is a list of places of interest, PairDists is a list of the stortest
%  paths between each POI, and Length is the total length of the route.
route_length(Route, PairDists, Length) :-
  pairwise(Route, RoutePairs),
  maplist(
    {PairDists}/[Start-End, Dist]>>memberchk(Start-End-Dist, PairDists),
    RoutePairs,
    RouteDists),
  sum_list(RouteDists, Length).

%% pairwise(+List, -Pairs) is det
%  Pairs is the pairwise grouping of the elements in List, e.g.
%  pairwise([a, b, c, d], [a-b, b-c, c-d]).
pairwise([], []).
pairwise([_], []).
pairwise([X, Y|T], [X-Y|R]) :- pairwise([Y|T], R).

% Input grammar
map_dcg([Chars|Rest]) -->
  nonblanks(Codes),
  { length(Codes, L), L #> 0, maplist(char_code, Chars, Codes) },
  blanks,
  map_dcg(Rest).
map_dcg([]) --> blanks.
