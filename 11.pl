#! /usr/bin/env swipl

% Solver for Day 11 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/11

:- initialization(main, main).
:- ensure_loaded('common.pl').

:- use_module(library(dcg/high_order)).

main(['1']) :- read_input(lines_dcg(Floors)), part1(Floors, R), writeln(R).
main(['2']) :- read_input(lines_dcg(Floors)), part2(Floors, R), writeln(R).
main(_) :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(Floors, R) :-
  msort(Floors, FloorsSorted),
  pairs_values(FloorsSorted, FloorItems),
  a_star(state(1, FloorItems), next_state, estimate_steps_remaining, goal_state,
    canonical_state, _, R).

part2(Floors, R) :-
  msort(Floors, [1-Floor1|UpperFloors]),
  EG = generator(elerium), EM = microchip(elerium),
  DG = generator(dilithium), DM = microchip(dilithium),
  part1([1-[EG, EM, DG, DM|Floor1]|UpperFloors], R).

%% a_star(+Init, :NextState, :Heuristic, :IsGoal, :Canonical, -Final, -Cost)
%  Performs an A* search given the following parameters:
%    * Init - the initial state
%    * NextState - a predicate that generates successor states on backtracking
%    * Heuristic - a predicate that calculates the heuristic value for a state
%    * IsGoal - a predicate that checks whether a state is a valid goal state
%    * Canonical - a predicate that converts a state to a canonical
%        representation (e.g. a hash function), for use in avoiding previously
%        visited states. Useful for pruning the search space if the problem has
%        many equivalent but non-identical states. If not required, (=) can be
%        used as an identity predicate.
%    * Final - the first goal state encountered. Returns successive, higher cost
%        goals on backtracking.
%    * Cost - the number of steps taken to reach Final
%  Fails if the search space is fully expanded without encountering a goal
%  state. For Goal to have minimum Cost, Heuristic should be optimistic.
a_star(Init, NextState, Heuristic, IsGoal, Canonical, Final, Cost) :-
  call(Heuristic, Init, H),
  list_to_assoc([H-(0-Init)], PQ),
  empty_assoc(Seen),
  a_star(PQ, Seen, NextState, Heuristic, IsGoal, Canonical, Final, Cost).

a_star(PQ, _, _, _, IsGoal, _, State, Step) :-
  del_min_assoc(PQ, _, Step-State, _),
  call(IsGoal, State).
a_star(PQ, Seen, NextState, Heuristic, IsGoal, Canonical, Final, Cost) :-
  del_min_assoc(PQ, _, Step-State, PQ1),
  call(Canonical, State, StateC),
  put_assoc(StateC, Seen, _, Seen1),
  findall(Next, call(NextState, State, Next), NextStates),
  Step1 #= Step + 1,
  maplist(
    {Canonical, Heuristic, Step1}/[S, (P-SC)-(Step1-S)]>>(
      call(Canonical, S, SC),
      call(Heuristic, S, H),
      P #= H + Step1),
    NextStates, NextStates1),
  exclude([(_-SC)-_]>>(get_assoc(SC, Seen1, _)), NextStates1, NextStates2),
  put_all_assoc(NextStates2, PQ1, PQ2),
  a_star(PQ2, Seen1, NextState, Heuristic, IsGoal, Canonical, Final, Cost).

%% put_all_assoc(+Pairs, +Assoc, -Assoc1) is det
%  Assoc1 is Assoc, but with the key-value pairs in Pairs added as associations.
%  If multiple items in Pairs have the same key value, later items replace
%  earlier ones.
put_all_assoc(Pairs, Assoc, Assoc1) :-
  foldl([K-V, A, A1]>>put_assoc(K, A, V, A1), Pairs, Assoc, Assoc1).

%% next_state(+State, -State1) is multi
%  State1 is a possible successor state of State. Generates all valid successors
%  on backtracking.
next_state(state(Elevator, Floors), state(Elevator1, Floors2)) :-
  next_floor(state(Elevator, Floors), Elevator1),
  nth1(Elevator, Floors, CurrentFloorItems),
  nth1(Elevator1, Floors, NextFloorItems),
  select_items_to_move(CurrentFloorItems, ElevatorItems, CurrentFloorItems1),
  append(ElevatorItems, NextFloorItems, NextFloorItems1),
  mutually_safe(NextFloorItems1),
  set_nth1(Elevator, Floors, CurrentFloorItems1, Floors1),
  set_nth1(Elevator1, Floors1, NextFloorItems1, Floors2).

%% next_floor(+State, -N) is multi
%  N is a floor to which the elevator can move from State. Generates all valid
%  destination floors on backtracking.
next_floor(state(E, _), N) :- E #> 1, N #= E - 1.
next_floor(state(E, Fs), N) :- length(Fs, L), E #< L, N #= E + 1.

%% select_items_to_move(+Items, -Move, -Stay) is nondet
%  Move/Stay is a partitioning of Items such that Move will fit in the elevator,
%  and neither group has components in danger of irradiation. Generates all
%  possible partitionings on backtracking. Fails if Items is empty.
select_items_to_move(Items, Move, Stay) :- pick_items(1, Items, Move, Stay).
select_items_to_move(Items, Move, Stay) :- pick_items(2, Items, Move, Stay).

%% estimate_steps_remaining(+State, -N) is det
%  N is an optimistic estimate of the number of steps remaining to get all items
%  in State to the top floor. The optimistic assumptions here are that the
%  elevator can teleport downwards, and always takes full loads without danger
%  of irradiation.
estimate_steps_remaining(state(_, Floors), N) :-
  length(Floors, L),
  numlist(1, L, FloorNumbers),
  maplist(
    {L}/[Floor, Items, Steps]>>(
      length(Items, NumItems),
      NumElevatorLoads #= (NumItems + 1) // 2,
      NumFloorsToTop #= L - Floor,
      Steps #= NumElevatorLoads * NumFloorsToTop),
    FloorNumbers, Floors, StepsByFloor),
  sum_list(StepsByFloor, N).

%% goal_state(+State) is semidet
%  True if all items are on the top floor.
goal_state(state(Elevator, Floors)) :-
  length(Floors, Elevator),
  nth1(Elevator, Floors, [_|_], LowerFloors),
  maplist(=([]), LowerFloors).

%% canonical_state(+State, -Canonical) is det
%  Canonical is the canonical form of State, in which the floors component is
%  replaced by a list of integer configuration summaries. Different states may
%  have the same canonical form if they are equivalent under relabelling of
%  elements and reordering of floor items. This is critical for search pruning
%  as the state space grows VERY quickly and contains a lot of different but
%  equivalent states (even with this optimisation Part 2 takes over 12 minutes
%  on my machine).
%
%  Each configuration integer is a base 4 representation of an element's layout
%  across the floors, with the lowest floor forming the most significant digit,
%  and microchips being a 1 and generators a 2. So for example, given the floor
%  plan: [[generator(a), microchip(b)], [generator(b)], [microchip(a)], []],
%  element a scores 2010 (base 4) = 132 (base 10), and
%  element b scores 1200 (base 4) = 96 (base 10). The list of integers is then
%  sorted to preserve equivalence under element relabelling.
canonical_state(state(Elevator, Floors), state(Elevator, FloorsC)) :-
  empty_assoc(Acc),
  canonical_floors(Floors, Acc, FloorsC).

canonical_floors([], Acc, Canonical) :-
  assoc_to_values(Acc, IntRepr),
  msort(IntRepr, Canonical).
canonical_floors([F|Fs], Acc, Canonical) :-
  map_assoc([V, V1]>>(V1 #= V << 2), Acc, Acc1),
  foldl(
    [I, A1, A2]>>(
      item_canonical_details(I, Element, Score),
      get_default_assoc(Element, 0, A1, V),
      V1 #= V + Score,
      put_assoc(Element, A1, V1, A2)),
    F, Acc1, Acc2),
  canonical_floors(Fs, Acc2, Canonical), !.

item_canonical_details(microchip(X), X, 1).
item_canonical_details(generator(X), X, 2).

%% select_n(?N, ?List, ?Choice, ?Remainder)
%  Is true when List, minus the N items in Choice, is Remainder. The relative
%  ordering of items is consistent between List, Choice and Remainder.
select_n(0, L, [], L).
select_n(N, List, [H|Choice], Remainder) :-
  N #> 0, N1 #= N-1,
  append(Init, [H|T], List),
  select_n(N1, T, Choice, NotChoice),
  append(Init, NotChoice, Remainder).

%% pick_items(+N, +Items, -Choice, -Remainder) is nondet
%  True when Choice/Remainder is a partitioning of Items such that the length of
%  Choice is N, and neither partition has components in danger of irradiation.
pick_items(N, Items, Choice, Remainder) :-
  select_n(N, Items, Choice, Remainder),
  mutually_safe(Choice),
  mutually_safe(Remainder).

%% mutually_safe(+Items) is semidet
%  True if all Items are safe from irradiation, i.e. there are no microchips
%  without a matching generator in the presence of a mismatched generator.
mutually_safe(Items) :-
  \+ (
    member(microchip(X), Items), \+ member(generator(X), Items),
    member(generator(Y), Items), X \= Y).

% Input grammar
lines_dcg(Floors) --> lines_dcg(1, Floors).
lines_dcg(N, [N-Items|Floors]) -->
  line_dcg(Items), blanks, lines_dcg(N1, Floors), { N1 #= N+1 }.
lines_dcg(_, []) --> remainder(_).

line_dcg(Items) -->
  "The ", nonblanks(_), " floor contains ", items_dcg(Items), ".".

items_dcg([]) --> "nothing relevant".
items_dcg([I]) --> item_dcg(I).
items_dcg([I|Is]) -->
  item_dcg(I), optional(",", []), optional(" and", []), blanks, items_dcg(Is).

item_dcg(microchip(Element)) -->
  "a ", string_without("- ", Codes), "-compatible microchip",
  { atom_codes(Element, Codes) }.
item_dcg(generator(Element)) -->
  "a ", string_without("- ", Codes), " generator",
  { atom_codes(Element, Codes) }.
