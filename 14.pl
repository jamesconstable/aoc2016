#! /usr/bin/env swipl

% Solver for Day 14 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/14

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, read_input((nonblanks(S), blanks)), part1(S, I), writeln(I).
main(['2']) :- !, read_input((nonblanks(S), blanks)), part2(S, I), writeln(I).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(Salt, LastIndex) :-
  findnsols(64, Index, otp_index(1, Salt, Index), KeyIndices),
  last(KeyIndices, LastIndex).

part2(Salt, LastIndex) :-
  findnsols(64, Index, otp_index(2017, Salt, Index), KeyIndices),
  last(KeyIndices, LastIndex).

%% otp_index(+Stretch, +Salt, -Index) is multi
%  Index is the index of a valid OTP for the given Salt and hash Stretch factor.
%  Generates additional valid indices on backtracking.
otp_index(Stretch, Salt, Index) :-
  between(0, infinite, Index),
  valid_key(Stretch, Salt, Index).

%% valid_key(+Stretch, +Salt, +N) is semidet
%  The concatenation of Salt and N is a valid OTP key for the given hash Stretch
%  factor.
valid_key(Stretch, Salt, N) :-
  hash(Stretch, Salt, N, Hash),
  append(_, [X, X, X|_], Hash), !,
  N1 #= N + 1, N1000 #= N + 1000,
  between(N1, N1000, I),
  hash(Stretch, Salt, I, HashI),
  append(_, [X, X, X, X, X|_], HashI).

:- table hash/4.

%% hash(+Stretch, +Salt, +N, -Hash) is det
%  Hash is the concatenation of Salt and N after being MD5 hashed Stretch times.
hash(Stretch, Salt, N, Hash) :-
  number_codes(N, NCodes),
  append(Salt, NCodes, Key),
  iterate(Stretch, [I, O]>>md5_hash(I, O, [encoding(utf8)]), Key, HashAtom),
  atom_chars(HashAtom, Hash).

%% iterate(+N, :Goal, +In, -Out)
%  Performs N iterations of Goal(In, Out), with the Out from each iteration
%  being used as the In of the next.
iterate(0, _, In, In) :- !.
iterate(N, Goal, In, Out) :-
  N #> 0, call(Goal, In, In1), N1 #= N-1, iterate(N1, Goal, In1, Out).
