#! /usr/bin/env swipl

% Solver for Day 10 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/10

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- read_input(instructions_dcg(I, G)), part1(I, G, B), writeln(B).
main(['2']) :- read_input(instructions_dcg(I, G)), part2(I, G, R), writeln(R).
main(_) :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

% The state functor used in this problem has the structure
% state(Bots:assoc, Outputs:assoc, Ready:list), where Bots is an assoc mapping
% bot numbers to bots, Outputs is an assoc mapping output bin numbers to a list
% of their contents, and Ready is a list of bots that are currently holding two
% chips, and are therefore ready to act. Bots are represented by a functor with
% the structure bot(give(BotNumber, LowDest, HighDest), ChipsHeld).

part1(Inits, Rules, Bot) :-
  initialise(Inits, Rules, State),
  execute_until(
    [state(Bots, _, [Curr|_])]>>(
      get_assoc(Curr, Bots, bot(_, Chips)),
      msort(Chips, [17, 61])),
    State, state(_, _, [Bot|_])).

part2(Inits, Rules, Result) :-
  initialise(Inits, Rules, State),
  execute_until(
    [state(_, Os, _)]>>(
      get_assoc(0, Os, [_|_]),
      get_assoc(1, Os, [_|_]),
      get_assoc(2, Os, [_|_])),
    State, state(_, Outputs, _)),
  assoc_to_list(Outputs, [0-[A|_], 1-[B|_], 2-[C|_]|_]),
  Result #= A * B * C.

%% initialise(+Inits, +Rules, -State)
%  Initialises the first state of the simulation from a list of initial chip
%  allocations, Inits, and a list of state update rules, Rules.
initialise(Inits, Rules, State) :-
  empty_assoc(Bots), empty_assoc(Outputs),
  initialise(Inits, Rules, state(Bots, Outputs, []), State).

initialise([init(Chip, Bot)|Is], Rs, state(Bots, Os, Ready), S) :-
  get_default_assoc(Bot, bot(_, []), Bots, bot(R, Chips)),
  put_assoc(Bot, Bots, bot(R, [Chip|Chips]), Bots1),
  (Chips=[_] -> Ready1=[Bot|Ready]; Ready1=Ready),
  initialise(Is, Rs, state(Bots1, Os, Ready1), S).
initialise([], [R|Rs], state(Bots, Outputs, Ready), S) :-
  R = give(Bot, LowDest, HighDest),
  get_default_assoc(Bot, bot(R, []), Bots, bot(R, Chips)),
  put_assoc(Bot, Bots, bot(R, Chips), Bots1),
  include([O]>>(O=output(_)), [LowDest, HighDest], NewOutputs),
  maplist([O, O1]>>(O=output(O1)), NewOutputs, NewOutputs1),
  put_keys(NewOutputs1, Outputs, Outputs1),
  initialise([], Rs, state(Bots1, Outputs1, Ready), S).
initialise([], [], S, S).

%% put_keys(+Keys, +Assoc, -Assoc1) is det
%  Adds the items in Keys to Assoc, producing Assoc1. New keys are mapped to an
%  empty list; existing keys retain their original mapping.
put_keys([], Assoc, Assoc).
put_keys([X|Xs], Assoc, Assoc1) :-
  get_assoc(X, Assoc, _), !,
  put_keys(Xs, Assoc, Assoc1).
put_keys([X|Xs], Assoc, Assoc2) :-
  \+ get_assoc(X, Assoc, _),
  put_assoc(X, Assoc, [], Assoc1),
  put_keys(Xs, Assoc1, Assoc2).

%% execute_until(:Pred, +State, -State1) is nondet
%  Advances the simulation from State, until a State1 satisfying Pred is found.
%  On backtracking, ignores State1 and continues simulation until another
%  satisfying state is found, or the simulation fails out.
execute_until(Pred, State, State) :- call(Pred, State).
execute_until(Pred, State, Final) :-
  State = state(Bots, Outputs, [Curr|Ready]),
  get_assoc(Curr, Bots, bot(R, Chips)),
  R = give(_, LowDest, HighDest),
  msort(Chips, [Low, High]),
  put_assoc(Curr, Bots, bot(R, []), Bots1),
  give_chip(Low, LowDest, state(Bots1, Outputs, Ready), State1),
  give_chip(High, HighDest, State1, State2),
  execute_until(Pred, State2, Final).

give_chip(Chip, bot(Dest), state(Bs, Os, Rs), state(Bs1, Os, Rs1)) :-
  get_assoc(Dest, Bs, bot(R, Chips)),
  put_assoc(Dest, Bs, bot(R, [Chip|Chips]), Bs1),
  (Chips=[_] -> Rs1=[Dest|Rs]; Rs1=Rs).
give_chip(Chip, output(Dest), state(Bs, Os, Rs), state(Bs, Os1, Rs)) :-
  get_assoc(Dest, Os, Chips),
  put_assoc(Dest, Os, [Chip|Chips], Os1).

% Input grammar
instructions_dcg([I|Is], Gs) --> init_dcg(I), blanks, instructions_dcg(Is, Gs).
instructions_dcg(Is, [G|Gs]) --> give_dcg(G), blanks, instructions_dcg(Is, Gs).
instructions_dcg([], []) --> [].

init_dcg(init(Chip, Bot)) -->
  "value ", integer(Chip), " goes to bot ", integer(Bot).

give_dcg(give(Bot, LowDest, HighDest)) -->
  "bot ", integer(Bot),
  " gives low to ", dest_dcg(LowDest),
  " and high to ", dest_dcg(HighDest).

dest_dcg(bot(I)) --> "bot ", integer(I).
dest_dcg(output(I)) --> "output ", integer(I).
