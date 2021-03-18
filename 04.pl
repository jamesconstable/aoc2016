#! /usr/bin/env swipl

% Solver for Day 4 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/4

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- read_input(rooms_grammar(Rs)), part1(Rs, S), writeln(S).
main(['2']) :- read_input(rooms_grammar(Rs)), part2(Rs, S), writeln(S).
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

%% rotate_list(?Distance, ?List, ?Rotated)
%  Rotated is the forwards rotation of List by Distance. For example,
%  rotate_list(1, [a, b, c], X) -> X = [c, a, b]. Can be used in any mode,
%  though CLP(FD) membership constraints may be required if Distance is unbound.
rotate_list(Distance, List, Rotated) :-
  length(List, L),
  N #= Distance mod L,
  split_at(N, List, A, B),
  append(B, A, Rotated), !.

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

%% rle(+List, -RunLengths) is det
%  RunLengths is the run-length encoding of List, expressed in pairs.
%  For example, rle([a, a, b, c, c, c, a], X) -> X = [2-a, 1-b, 3-c, 1-a].
rle([], []).
rle(Xs, [L-E|Rs]) :- split_eqs(Xs, [E|Es], T), length([E|Es], L), rle(T, Rs).

%% split_eqs(+List, -Eqs, -Rest) is det
%  List is the concatenation of Eqs and Rest, where all elements of Eqs are
%  unifiable.
split_eqs([X], [X], []).
split_eqs([X, X|Xs], [X|Eqs], Rest) :- split_eqs([X|Xs], Eqs, Rest).
split_eqs([X1, X2|Xs], [X1], [X2|Xs]) :- X1 \= X2.

%% split_at(?Index, ?List, ?Init, ?Tail)
%  List is the concatenation of Init and Tail, where Init has length Index.
split_at(0, Xs, [], Xs).
split_at(N, [X|Xs], [X|A], B) :- N #> 0, N1 #= N-1, split_at(N1, Xs, A, B).

% Input grammar
room_grammar(room(Name, Sector, Checksum)) -->
  string(NameCodes), "-",
  integer(Sector), "[",
  string_without("]", ChecksumCodes), "]",
  { maplist(char_code, Name, NameCodes),
    maplist(char_code, Checksum, ChecksumCodes) }.
rooms_grammar([R|Rs]) --> room_grammar(R), "\n", rooms_grammar(Rs).
rooms_grammar([]) --> remainder(_).
