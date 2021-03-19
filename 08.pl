#! /usr/bin/env swipl

% Solver for Day 8 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/8

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :-
  read_input(instructions_dcg(Is)),
  execute(Is, 50, 6, Final),
  lit_count(Final, C),
  writeln(C).
main(['2']) :-
  read_input(instructions_dcg(Is)),
  execute(Is, 50, 6, Final),
  write_display(Final).
main(_) :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

%% blank(?W, ?H, ?B)
%  B is a list of H lists, each containing W '.' atoms.
blank(W, H, B) :- length(B, H), maplist(replicate(W, '.'), B).

%% replicate(?N, ?X, ?Xs)
%  Xs is a list of length N where all elements are X.
replicate(N, X, Xs) :- length(Xs, N), maplist(=(X), Xs).

%% execute(+Instructions, +W, +H, -Final) is det
%  Executes Instructions on a blank WxH display, with Final as the result.
execute(Is, W, H, Final) :- blank(W, H, B), foldl(update, Is, B, Final).

%% update(+Instruction, +Display, -Display1) is det
%  Display1 is Display after executing Instruction.
update(rect(W, H), Display, Display1) :-
  split_at(H, Display, RowsToChange, RowsToKeep),
  maplist(
    {W}/[R, R1]>>(
      split_at(W, R, _, CellsToKeep),
      replicate(W, '#', CellsToChange1),
      append(CellsToChange1, CellsToKeep, R1)),
    RowsToChange, RowsToChange1), !,
  append(RowsToChange1, RowsToKeep, Display1).
update(row(Y, V), Display, Display1) :-
  nth0(Y, Display, Row),
  NV #= -V,
  rotate_list(NV, Row, Row1),
  set_nth0(Y, Display, Row1, Display1), !.
update(col(X, V), Display, Display1) :-
  rows_cols(Display, DisplayT),
  update(row(X, V), DisplayT, DisplayT1),
  rows_cols(DisplayT1, Display1).

%% set_nth0(?N, ?Xs, ?V, ?Xs1)
%  Xs1 is Xs, but with the item at index N (zero-based) set to V.
set_nth0(0, [_|Xs], V, [V|Xs]).
set_nth0(N, [X|Xs], V, [X|Xs1]) :- N #> 0, N1 #= N-1, set_nth0(N1, Xs, V, Xs1).

%% lit_count(+Rs, -Count) is det
%  Rs is a list of lists containing Count '#' atoms.
lit_count(Rs, Count) :- maplist(count(=(#)), Rs, RCs), !, sum_list(RCs, Count).

%% write_display(+Display) is det
%  Pretty prints Display to the current output, using hashes and spaces.
write_display(Display) :-
  member(Row, Display),
  maplist([I, O]>>(I='.' -> O=' '; O=I), Row, Row1),
  atomic_list_concat(Row1, Line),
  writeln(Line),
  fail.
write_display(_).

% Input grammar
index_by_value_dcg(I, V) --> integer(I), " by ", integer(V).

instruction_dcg(rect(W, H)) --> "rect ", integer(W), "x", integer(H).
instruction_dcg(row(Y, V))  --> "rotate row y=", index_by_value_dcg(Y, V).
instruction_dcg(col(X, V))  --> "rotate column x=", index_by_value_dcg(X, V).

instructions_dcg([L|Ls]) --> instruction_dcg(L), blanks, instructions_dcg(Ls).
instructions_dcg([])     --> remainder(_).
