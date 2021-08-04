#! /usr/bin/env swipl

% Solver for Day 17 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/17

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, read_input(input_dcg(I)), solve_shortest(I, R), writeln(R).
main(['2']) :- !, read_input(input_dcg(I)), solve_longest(I, R), writeln(R).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

solve_shortest(Input, Path) :-
  bfs(state(0/0, Input), next_state, is_goal, (=), state(_, Final), _),
  append(Input, PathList, Final),
  atomic_list_concat(PathList, Path).

solve_longest(Input, Length) :-
  findall(L, dfs(state(0/0, Input), _, L), Ls),
  max_member(Length, Ls).

%% next_state(_, +State, -State1) is nondet
%  State1 is a valid successor state of State, where states are expressed in the
%  form: state(X/Y, KeyList). The first argument is unused, but is required by
%  the bfs/6 contract. Returns all possible successor states on backtracking;
%  fails immediately in a deadend.
next_state(_, State, State1) :-
  State = state(_, Key),
  md5_hash(Key, Hash, [encoding(utf8)]),
  atom_chars(Hash, [U, D, L, R|_]),
  maplist(
    [C, Door]>>(
      memberchk(C, [b, c, d, e, f])
        -> Door = open
        ;  Door = closed),
    [U, D, L, R], Doors),
  try_moves(Doors, State, State1).

%% try_moves(+Doors, +State, -State1) is nondet
%  State1 is a valid successor of State, given the door configuration Doors (a
%  4-element list containing open/closed terms corresponding to each of the
%  directions, UP, DOWN, LEFT and RIGHT, in that order). Tries all open doors
%  on backtracking; fails immediately in a deadend.
try_moves(_, state(3/3, _), _) :- !, fail.
try_moves([open, _, _, _], state(X/Y, Key), state(X/Y1, Key1)) :-
  Y #> 0,
  Y1 #= Y - 1,
  append(Key, ['U'], Key1).
try_moves([_, open, _, _], state(X/Y, Key), state(X/Y1, Key1)) :-
  Y #< 3,
  Y1 #= Y + 1,
  append(Key, ['D'], Key1).
try_moves([_, _, open, _], state(X/Y, Key), state(X1/Y, Key1)) :-
  X #> 0,
  X1 #= X - 1,
  append(Key, ['L'], Key1).
try_moves([_, _, _, open], state(X/Y, Key), state(X1/Y, Key1)) :-
  X #< 3,
  X1 #= X + 1,
  append(Key, ['R'], Key1).

%% is_goal(+State) is semidet
%  True if State is located in the bottom-right corner of the maze.
is_goal(state(3/3, _)).

%% dfs(+Init, -Goal, -Depth) is nondet
%  Goal is a valid goal state reachable from Init, via Depth steps. Returns
%  other possible paths on backtracking. In theory, bfs/6 could have been used
%  for finding the longest path too, but Prolog's assoc implementation is so
%  slow as to be impractical for this problem.
dfs(Init, Init, 0) :- is_goal(Init).
dfs(Init, Goal, N1) :-
  next_state(_, Init, Next),
  dfs(Next, Goal, N),
  N1 #= N + 1.

% Input grammar.
input_dcg(S) --> nonblanks(C), blanks, { maplist(char_code, S, C) }.
