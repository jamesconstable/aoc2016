#! /usr/bin/env swipl

% Solver for Day 9 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/9

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, read_input(decompress_dcg(L)), writeln(L).
main(['2']) :- !, read_input(decompress_dcg2(L)), writeln(L).
main(_) :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

%% space(+X) is semidet
%  Succeeds if X is any whitespace character.
space(X) :- code_type(X, space).

%% deblanked_dcg(?Length, ?Codes)// is multi
%  Take as few tokens as possible, taking one more each time on backtracking.
%  Whitespace characters are not counted in Length or included in Codes. Length
%  can be used to exactly specify the number of non-whitespace tokens taken.
deblanked_dcg(0, []) --> [].
deblanked_dcg(Length, Codes) -->
  [C], { space(C) }, !, deblanked_dcg(Length, Codes).
deblanked_dcg(Length, [C|Codes]) -->
  [C], { \+ space(C) }, deblanked_dcg(L, Codes), { Length #= L + 1 }.

% Decompression grammar for Part 1. Note that the return vars are the
% decompressed LENGTHS; this grammar does not construct the decompression.
decompress_dcg(L) -->
  deblanked_dcg(L1, _), decompress_block_dcg(L2), !, decompress_dcg(L3),
  { L #= L1 + L2 + L3 }.
decompress_dcg(L) --> deblanked_dcg(L, _).

decompress_block_dcg(L) -->
  "(", integer(Run), "x", integer(Times), ")", deblanked_dcg(Run, _),
  { L #= Run * Times }.

% Decompression grammar for Part 2. Again, this grammar measures the length of
% the decompression without constructing it.
decompress_dcg2(L) -->
  deblanked_dcg(L1, _), decompress_block_dcg2(L2), !, decompress_dcg2(L3),
  { L #= L1 + L2 + L3 }.
decompress_dcg2(L) --> deblanked_dcg(L, _).

decompress_block_dcg2(L) -->
  "(", integer(Run), "x", integer(Times), ")", deblanked_dcg(Run, Data),
  { phrase(decompress_dcg2(Nested), Data), L #= Nested * Times }.
