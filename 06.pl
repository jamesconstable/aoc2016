#! /usr/bin/env swipl

% Solver for Day 6 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/6

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- read_input(grammar(Ls)), decode(max_member, Ls, M), writeln(M).
main(['2']) :- read_input(grammar(Ls)), decode(min_member, Ls, M), writeln(M).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

%% decode(:Selector, +Ls, -Message) is det
%  Decodes the message list Ls into Message, using the predicate Selector to
%  choose each letter from a frequency-letter pair list.
decode(Selector, Ls, Message) :-
  rows_cols(Ls, Cols),
  maplist(
    {Selector}/[Col, Char]>>(
      msort(Col, ColSorted),
      rle(ColSorted, FreqChars),
      call(Selector, _-Char, FreqChars)),
    Cols, Chars),
  atom_chars(Message, Chars), !.

%% rows_cols(+Rows, -Cols) is det
%% rows_cols(-Rows, +Cols) is det
%  Rows is the transpose of Cols, where both are lists of lists.
rows_cols(Rows, []) :- maplist(=([]), Rows), !.
rows_cols(Rows, [Col|Cols]) :-
  maplist([[C|Cs], C, Cs]>>true, Rows, Col, Rows1),
  rows_cols(Rows1, Cols).

% Input grammar
grammar([])     --> [].
grammar([L|Ls]) -->
  nonblanks(Codes), blanks, grammar(Ls), { maplist(char_code, L, Codes) }.