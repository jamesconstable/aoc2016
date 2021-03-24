#! /usr/bin/env swipl

% Solver for Day 4 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/4

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, read_input(rooms_grammar(Rs)), part1(Rs, S), writeln(S).
main(['2']) :- !, read_input(rooms_grammar(Rs)), part2(Rs, S), writeln(S).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(Rooms, Sum) :-
  include(is_room, Rooms, RealRooms),
  maplist([R, S]>>(R = room(_, S, _)), RealRooms, Sectors),
  sum_list(Sectors, Sum).

part2(Rooms, Sector) :-
  include(is_room, Rooms, RealRooms),
  maplist(encrypt, Decrypted, RealRooms),
  atom_chars('northpole object storage', NPOS),
  memberchk(room(NPOS, Sector, _), Decrypted).

%% encrypt(?PlaintextRoom, ?EncryptedRoom) is det
%  EncryptedRoom is the encryption of PlaintextRoom, i.e. the characters in its
%  name have been Caesar shifted backwards by the sector number, and spaces have
%  been replaced by dashes.
encrypt(room(Plaintext, S, Cs), room(Encrypted, S, Cs)) :-
  atom_chars(abcdefghijklmnopqrstuvwxyz, Alphabet),
  rotate_list(-S, Alphabet, Rotated),
  maplist([A, R, A-R]>>true, Alphabet, Rotated, Rotmap),
  maplist(
    {Rotmap}/[P, E]>>memberchk(P-E, [' '-'-'|Rotmap]),
    Plaintext, Encrypted).

%% is_room(+Room) is semidet
%  True if Room is a non-decoy room.
is_room(room(Name, _, Checksum)) :-
  exclude(=('-'), Name, Letters),
  msort(Letters, Sorted),
  rle(Sorted, RLE),
  predsort(
    [O, C1-L1, C2-L2]>>(N1 #= -C1, N2 #= -C2, compare(O, N1-L1, N2-L2)),
    RLE, RLESorted),
  pairs_values(RLESorted, FullChecksum),
  split_at(5, FullChecksum, Checksum, _).

% Input grammar
room_grammar(room(Name, Sector, Checksum)) -->
  string(NameCodes), "-",
  integer(Sector), "[",
  string_without("]", ChecksumCodes), "]",
  { maplist(char_code, Name, NameCodes),
    maplist(char_code, Checksum, ChecksumCodes) }.
rooms_grammar([R|Rs]) --> room_grammar(R), "\n", rooms_grammar(Rs).
rooms_grammar([]) --> remainder(_).
