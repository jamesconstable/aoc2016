#! /usr/bin/env swipl

% Solver for Day 18 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/18

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, read_input(input_dcg(I)), part1(I, R), writeln(R).
main(['2']) :- !, read_input(input_dcg(I)), part2(I, R), writeln(R).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(I, C) :- safe_count_rows(I, 40, C).
part2(I, C) :- safe_count_rows(I, 400000, C).

safe_count_rows(I, NRows, SafeCount) :-
  iterate(NRows,
    [L-Acc, L1-Acc1]>>once((
      count(=('.'), L, N),
      line_succ(L, L1),
      Acc1 #= Acc + N)),
    I-0, _-SafeCount).

%% iterate(+N, :Goal, ?I, ?R) is det
%  R is the result of calling Goal N times, with I as the first argument of the
%  first call, and thereafter using the second argument of each call as the
%  first argument of the next.
iterate(0, _, I, I).
iterate(N, Goal, I, R) :-
  N #> 0, N1 #= N-1,
  call(Goal, I, I2),
  iterate(N1, Goal, I2, R).

%% line_succ(+L, -S)
%  S is the line following L according to the trap placement rules.
line_succ(L, S) :- append(['.'|L], ['.'], WithWalls), line_succ_(WithWalls, S).

line_succ_(['^', '^', '.'|T], ['^'|R]) :- line_succ_(['^', '.'|T], R).
line_succ_(['.', '^', '^'|T], ['^'|R]) :- line_succ_(['^', '^'|T], R).
line_succ_(['^', '.', '.'|T], ['^'|R]) :- line_succ_(['.', '.'|T], R).
line_succ_(['.', '.', '^'|T], ['^'|R]) :- line_succ_(['.', '^'|T], R).
line_succ_([_, A, B|T],       ['.'|R]) :- line_succ_([A, B|T], R).
line_succ_([_, _],            []).

% Input grammar.
input_dcg(S) --> nonblanks(C), blanks, { maplist(char_code, S, C) }.
