#! /usr/bin/env swipl

% Solver for Day 16 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/16

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, solve_for(272).
main(['2']) :- !, solve_for(35651584).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

solve_for(N) :- read_input(binary_dcg(I)), dragon_checksum(I, N, C), writeln(C).

%% dragon_checksum(+Init, +FileSize, -Checksum) is det
%  Checksum is the checksum of the first FileSize tokens of the dragon sequence
%  formed from the base sequence Init. This solution runs in constant memory,
%  but is quite slow for large inputs (computing the checksum for a 35MB file
%  takes about 9 minutes on my machine).
dragon_checksum(Init, FileSize, Checksum) :-
  dragon_sequence(Init, DragonSequence),
  checksum_chunk_size(FileSize, ChunkSize),
  NumChunks #= FileSize // ChunkSize,
  checksum(NumChunks, ChunkSize, DragonSequence, ChecksumDigits),
  atomic_list_concat(ChecksumDigits, Checksum).

%% checksum(+NumChunks, +ChunkSize, +List, -Checksum) is det
%  Checksum is a list of length NumChunks containing the checksum digits for
%  List, when processed in chunks of ChunkSize (must be a power of 2). List may
%  be longer than NumChunks * ChunkSize, and infinite lazy lists are acceptable.
%  To run in constant memory, this solution relies on the observation that the
%  checksum algorithm simplifies to a parity check, which can be done by NXORing
%  the individual bits into an accumulator.
checksum(NumChunks, ChunkSize, [X|Xs], Checksum) :-
  I #= ChunkSize - 1,
  checksum(NumChunks, ChunkSize, I, Xs, Checksum, X).

checksum(0, _, 0, _, [], _).
checksum(NumChunks, ChunkSize, 0, [X|Xs], [Acc|T], Acc) :-
  NumChunks > 0,
  NumChunks1 #= NumChunks - 1,
  I #= ChunkSize - 1, !,
  checksum(NumChunks1, ChunkSize, I, Xs, T, X).
checksum(NumChunks, ChunkSize, I, [X|Xs], R, Acc) :-
  I > 0,
  I1 #= I - 1,
  nxor(Acc, X, Acc1),
  checksum(NumChunks, ChunkSize, I1, Xs, R, Acc1).

%% checksum_chunk_size(+FileSize, -ChunkSize) is det
%  ChunkSize is the largest power of 2 that divides evenly into FileSize.
checksum_chunk_size(FileSize, 1) :- FileSize mod 2 #= 1, !.
checksum_chunk_size(FileSize, N) :-
  FileSize mod 2 #= 0,
  checksum_chunk_size(FileSize // 2, N1),
  N #= N1 * 2.

%% nxor(?X, ?Y, ?R) is det
%  R is the negated exclusive-OR of X and Y.
nxor(X, X, 1) :- !.
nxor(X, Y, 0) :- X #\= Y.

%% dragon_sequence(+Init, -List) is det
%  List is an infinite lazy list of the digits of the dragon sequence formed
%  from the base sequence Init. The key observation that permits this is that
%  the list takes the form I-S0-R-S1-I-S2-R-S3-I-..., where I is the Init
%  sequence, R is its negated reverse, and SN is the Nth separator digit. The
%  pattern in the separator digits can be deduced by looking at their
%  arrangement as a binary tree; effectively, SN is the digit immediately left
%  of the rightmost zero in the binary representation of N.
dragon_sequence(Init, List) :-
  reverse(Init, Rev),
  maplist([X, N]>>(X = 0 -> N = 1; N = 0), Rev, RevNeg),
  lazy_list(dragon_gen(Init, RevNeg), state(true, 0, []), List).

dragon_gen([X|Xs], _, state(true, 0, []), state(false, 0, Xs), X).
dragon_gen(_, _, state(P, I, [N|Ns]), state(P, I, Ns), N).
dragon_gen(Sequence, RevNeg, state(P, I, []), state(P1, I1, Next), S) :-
  dragon_separator(I, S),
  I1 #= I + 1,
  (P -> Next = Sequence, P1 = false
     ;  Next = RevNeg, P1 = true).

dragon_separator(I, S) :- I /\ 1 #= 0, (I >> 1) /\ 1 #= S.
dragon_separator(I, S) :- I /\ 1 #= 1, dragon_separator(I >> 1, S).

% Input grammar.
binary_dcg(Dragon) -->
  nonblanks(DragonCodes), blanks,
  { maplist([C, N]>>number_codes(N, [C]), DragonCodes, Dragon) }.
