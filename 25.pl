#! /usr/bin/env swipl

% Solver for Day 25 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/25

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, read_input(instructions_dcg(Is)), part1(Is, R), writeln(R).
main(['2']) :- !, writeln('The End, no Part 2 :)').
main(_) :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(Instructions, N) :-
  instructions_to_program(Instructions, Program, L),
  between(1, 1000000, N),
  execute(Program, L, 1, _{a: N, b: 0, c: 0, d: 0, output: []}, After),
  alternating(After.output).

%% instructions_to_program(+Instructions, -Program, -Length) is det
%  Program is a line-number indexed assoc formed from Instructions, and Length
%  is the number of instructions.
instructions_to_program(Instructions, Program, Length) :-
  length(Instructions, Length),
  numlist(1, Length, LineNums),
  maplist([L, I, P]>>(P = L-I), LineNums, Instructions, ProgramPairs),
  ord_list_to_assoc(ProgramPairs, Program).

%% execute(+Program, +NumLines, +Line, +RegistersIn, -RegistersOut) is det
%  Executes Program starting at Line, with registers initialised to the values
%  in RegistersIn. NumLines is the number of lines in the program, and
%  RegistersOut is the final state of the registers when the program halts.
execute(_, NumLines, Line, Rs, Rs) :- (Line #< 0; Line #> NumLines), !.
execute(Program, NumLines, Line, RegistersIn, RegistersOut) :-
  Line #> 0,
  Line #=< NumLines,
  get_assoc(Line, Program, Instruction),
  execute_instruction(Instruction, Line, Line1, RegistersIn, Registers1),
  execute(Program, NumLines, Line1, Registers1, RegistersOut).

%% execute_instruction(+Instruction, +L, -L1, +Rs, -Rs1) is det
%  L1 is the new line number and Rs1 the new register state after executing
%  Instruction from line L with initial Rs.
execute_instruction(cpy(_, v(_)), L, L1, Rs, Rs) :-
  L1 #= L + 1.
execute_instruction(cpy(Arg, r(Dest)), L, L1, Rs, Rs1) :-
  arg_num(Arg, Rs, V),
  Rs1 = Rs.put(Dest, V),
  L1 #= L + 1.
execute_instruction(inc(v(_)), L, L1, Rs, Rs) :-
  L1 #= L + 1.
execute_instruction(inc(r(R)), L, L1, Rs, Rs1) :-
  V is Rs.R + 1,
  Rs1 = Rs.put(R, V),
  L1 #= L + 1.
execute_instruction(dec(v(_)), L, L1, Rs, Rs) :-
  L1 #= L + 1.
execute_instruction(dec(r(R)), L, L1, Rs, Rs1) :-
  V is Rs.R - 1,
  Rs1 = Rs.put(R, V),
  L1 #= L + 1.
execute_instruction(jnz(TestArg, _), L, L1, Rs, Rs) :-
  arg_num(TestArg, Rs, TestVal),
  TestVal #= 0,
  L1 #= L + 1.
execute_instruction(jnz(TestArg, DistArg), L, L1, Rs, Rs) :-
  arg_num(TestArg, Rs, TestVal),
  arg_num(DistArg, Rs, DistVal),
  TestVal #\= 0,
  L1 #= L + DistVal.
execute_instruction(out(Arg), L, L1, Rs, Rs1) :-
  arg_num(Arg, Rs, ArgVal),
  Rs1 = Rs.put(output, [ArgVal|Rs.output]),
  length(Rs1.output, Length),
  ((Rs1.output = [X, X|_]; Length #>= 100)
    -> L1 #= -1
    ;  L1 #= L + 1).

%% arg_num(+Arg, +Registers, -Value) is det
%  Extracts the Value of Arg, either directly in the case of a v functor, or
%  through a Registers lookup in the case of an r functor.
arg_num(v(V), _, V).
arg_num(r(R), Rs, Rs.R).

%% alternating(+List) is semidet
%  Succeeds if List is an alternating sequence of 0s and 1s.
alternating([]).
alternating([_]).
alternating([1, 0|T]) :- alternating([0|T]).
alternating([0, 1|T]) :- alternating([1|T]).

% Input grammar
instruction_dcg(cpy(V, Dest)) --> "cpy ", arg_dcg(V), " ", arg_dcg(Dest).
instruction_dcg(inc(R))       --> "inc ", arg_dcg(R).
instruction_dcg(dec(R))       --> "dec ", arg_dcg(R).
instruction_dcg(jnz(T, Dist)) --> "jnz ", arg_dcg(T), " ", arg_dcg(Dist).
instruction_dcg(out(V))       --> "out ", arg_dcg(V).

arg_dcg(v(V)) --> integer(V).
arg_dcg(r(R)) --> nonblanks(Cs), { atom_codes(R, Cs) }.

instructions_dcg([I|Is]) --> instruction_dcg(I), blanks, instructions_dcg(Is).
instructions_dcg([])     --> [].
