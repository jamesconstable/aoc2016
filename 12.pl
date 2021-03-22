#! /usr/bin/env swipl

% Solver for Day 12 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/12

:- initialization(main, main).
:- ensure_loaded('common.pl').

:- use_module(library(dcg/high_order)).

main(['1']) :- read_input(instructions_dcg(Is)), part1(Is, R), writeln(R).
main(['2']) :- read_input(instructions_dcg(Is)), part2(Is, R), writeln(R).
main(_) :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(Instructions, A) :-
  instructions_to_program(Instructions, Program, L),
  execute(Program, L, 1, [0, 0, 0, 0], [A|_]).

part2(Instructions, A) :-
  instructions_to_program(Instructions, Program, L),
  execute(Program, L, 1, [0, 0, 1, 0], [A|_]).

%% instructions_to_program(+Instructions, -Program, -Length) is det
%  Program is a line-number indexed dict formed from Instructions, and Length
%  is the number of instructions.
instructions_to_program(Instructions, Program, Length) :-
  length(Instructions, Length),
  numlist(1, Length, LineNums),
  maplist([L, I, P]>>(P = L-I), LineNums, Instructions, ProgramPairs),
  dict_create(Program, program, ProgramPairs).

%% execute(+Program, +NumLines, +Line, +RegistersIn, -RegistersOut) is det
%  Executes Program starting at Line, with registers initialised to the values
%  in RegistersIn. NumLines is the number of lines in the program, and
%  RegistersOut is the final state of the registers when the program halts.
execute(_, NumLines, Line, Rs, Rs) :- Line #> NumLines, !.
execute(Program, NumLines, Line, RegistersIn, RegistersOut) :-
  Line #=< NumLines,
  execute_instruction(Program.Line, Line, Line1, RegistersIn, Registers1),
  execute(Program, NumLines, Line1, Registers1, RegistersOut).

%% execute_instruction(+Instruction, +L, -L1, +Registers, -Registers1) is det
%  L1 is the new line number and Registers1 the new register state after
%  executing Instruction from line L with initial Registers.
execute_instruction(cpy_val(V, Dest), L, L1, Rs, Rs1) :-
  set_register(Dest, Rs, V, Rs1),
  L1 #= L + 1.
execute_instruction(cpy_reg(R, Dest), L, L1, Rs, Rs1) :-
  get_register(R, Rs, V),
  set_register(Dest, Rs, V, Rs1),
  L1 #= L + 1.
execute_instruction(inc(R), L, L1, Rs, Rs1) :-
  get_register(R, Rs, V),
  V1 #= V + 1,
  set_register(R, Rs, V1, Rs1),
  L1 #= L + 1.
execute_instruction(dec(R), L, L1, Rs, Rs1) :-
  get_register(R, Rs, V),
  V1 #= V - 1,
  set_register(R, Rs, V1, Rs1),
  L1 #= L + 1.
execute_instruction(jnz_val(0, _), L, L1, Rs, Rs) :-
  L1 #= L + 1.
execute_instruction(jnz_val(N, D), L, L1, Rs, Rs) :-
  N #\= 0,
  L1 #= L + D.
execute_instruction(jnz_reg(R, _), L, L1, Rs, Rs) :-
  get_register(R, Rs, 0),
  L1 #= L + 1.
execute_instruction(jnz_reg(R, D), L, L1, Rs, Rs) :-
  get_register(R, Rs, V),
  V #\= 0,
  L1 #= L + D.

%% get_register(?Register, ?Registers, ?Value) is det
%  Value is the current value of Register (a, b, c, or d) in Registers.
get_register(a, [A, _, _, _], A).
get_register(b, [_, B, _, _], B).
get_register(c, [_, _, C, _], C).
get_register(d, [_, _, _, D], D).

%% set_register(?Register, ?Registers, ?Value, ?Registers1) is det
%  Registers1 is Registers, but with Register (a, b, c, or d) set to Value.
set_register(a, [_, B, C, D], A, [A, B, C, D]).
set_register(b, [A, _, C, D], B, [A, B, C, D]).
set_register(c, [A, B, _, D], C, [A, B, C, D]).
set_register(d, [A, B, C, _], D, [A, B, C, D]).

% Input grammar
instruction_dcg(cpy_val(V, Dest)) -->
  "cpy ", integer(V), " ", nonblanks(Cs),
  { atom_codes(Dest, Cs) }.
instruction_dcg(cpy_reg(R, Dest)) -->
  "cpy ", nonblanks(RCs), " ", nonblanks(DestCs),
  { atom_codes(R, RCs), atom_codes(Dest, DestCs) }.
instruction_dcg(inc(R)) --> "inc ", nonblanks(Cs), { atom_codes(R, Cs) }.
instruction_dcg(dec(R)) --> "dec ", nonblanks(Cs), { atom_codes(R, Cs) }.
instruction_dcg(jnz_val(N, V)) --> "jnz ", integer(N), " ", integer(V).
instruction_dcg(jnz_reg(R, V)) -->
  "jnz ", nonblanks(Cs), " ", integer(V), { atom_codes(R, Cs) }.

instructions_dcg([I|Is]) --> instruction_dcg(I), blanks, instructions_dcg(Is).
instructions_dcg([]) --> [].
