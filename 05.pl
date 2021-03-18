#! /usr/bin/env swipl

% Solver for Day 5 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/5

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- read_input(grammar(Id)), part1(Id, Password), writeln(Password).
main(['2']) :- read_input(grammar(Id)), part2(Id, Password), writeln(Password).
main(_)     :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(Id, Password) :- length(Cs, 8), crack(Id, Cs), atom_chars(Password, Cs).
part2(Id, Password) :- length(Cs, 8), crack2(Id, Cs), atom_chars(Password, Cs).

%% crack(+Prefix, ?Cs) is det
%  Given a Prefix for the first hashing algorithm and a skeleton list, Cs, binds
%  the slots in Cs to their password character values.
crack(Prefix, Cs) :- crack(Prefix, 0, Cs), !.
crack(_, _, []).
crack(Prefix, Start, [C|Cs]) :-
  generate_hashes(Prefix, Start, Hash, I),
  atom_chars(Hash, ['0','0','0','0','0',C|_]),
  Start1 #= I + 1,
  crack(Prefix, Start1, Cs).

%% crack2(+Prefix, ?Cs) is det
%  Given a Prefix for the second hashing algorithm and a skeleton list, Cs,
%  binds the slots in Cs to their password character values.
crack2(Prefix, Cs) :- crack2(Prefix, 0, Cs), !.
crack2(_, _, Cs) :- ground(Cs).
crack2(Prefix, Start, Cs) :-
  generate_hashes(Prefix, Start, Hash, I),
  atom_chars(Hash, ['0','0','0','0','0',N,V|_]),
  set_password_char(N, V, Cs),
  Start1 #= I + 1,
  crack2(Prefix, Start1, Cs).

%% generate_hashes(+Prefix, +Start, -Hash, -I) is multi
%  Given a Prefix and a Start index, generates successive Hash values with their
%  corresponding index, I.
generate_hashes(Prefix, Start, Hash, I) :-
  append(Prefix, IHole, HashItem),
  between(Start, infinite, I),
  number_codes(I, IHole),
  md5_hash(HashItem, Hash, [encoding(utf8)]).

%% set_password_char(+N, +V, ?Cs) is semidet
%  Binds position N (zero-based) in Cs to V. False if N cannot be converted to
%  a number or is out of range for Cs.
set_password_char(N, V, Cs) :-
  char_type(N, digit), number_chars(I, [N]), nth0(I, Cs, V).

% Input grammar
grammar(Id) --> nonblanks(Id), remainder(_).
