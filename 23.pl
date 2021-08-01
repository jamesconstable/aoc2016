#! /usr/bin/env swipl

% Solver for Day 23 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/23

% I didn't go down the assembly optimisation path for Part 2; while tracing the
% execution of Part 1, I noticed it was computing the factorial of the value in
% in register A, plus some other value, so part 2 just calculates 12! and adds
% the same offset found in Part 1.

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, read_input(instructions_dcg(Is)), part1(Is, R), writeln(R).
main(['2']) :- !, part2(R), writeln(R).
main(_) :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

part1(Instructions, A) :-
  instructions_to_program(Instructions, Program, L),
  execute(Program, L, 1, _{a:7, b:0, c:0, d:0}, _{a:A, b:_, c:_, d:_}).

part2(A) :- factorial(12, F), A #= F + 7663.

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
execute(_, NumLines, Line, Rs, Rs) :- Line #> NumLines, !.
execute(Program, NumLines, Line, RegistersIn, RegistersOut) :-
  Line #=< NumLines,
  get_assoc(Line, Program, Instruction),
  execute_instruction(Instruction, Line, Line1, RegistersIn, Registers1,
    Program, Program1),
  execute(Program1, NumLines, Line1, Registers1, RegistersOut).

%% execute_instruction(+Instruction, +Line, -Line1, +Registers, -Registers1,
%%   +Program, -Program1) is det
%  L1 is the new line number, Registers1 the new register state and Program1
%  the new program assoc after executing Instruction from line L with initial
%  Registers.
execute_instruction(cpy(_, v(_)), L, L1, Rs, Rs, P, P) :-
  L1 #= L + 1.
execute_instruction(cpy(Arg, r(Dest)), L, L1, Rs, Rs1, P, P) :-
  arg_num(Arg, Rs, V),
  Rs1 = Rs.put(Dest, V),
  L1 #= L + 1.
execute_instruction(inc(v(_)), L, L1, Rs, Rs, P, P) :-
  L1 #= L + 1.
execute_instruction(inc(r(R)), L, L1, Rs, Rs1, P, P) :-
  V is Rs.R + 1,
  Rs1 = Rs.put(R, V),
  L1 #= L + 1.
execute_instruction(dec(v(_)), L, L1, Rs, Rs, P, P) :-
  L1 #= L + 1.
execute_instruction(dec(r(R)), L, L1, Rs, Rs1, P, P) :-
  V is Rs.R - 1,
  Rs1 = Rs.put(R, V),
  L1 #= L + 1.
execute_instruction(jnz(TestArg, _), L, L1, Rs, Rs, P, P) :-
  arg_num(TestArg, Rs, TestVal),
  TestVal #= 0,
  L1 #= L + 1.
execute_instruction(jnz(TestArg, DistArg), L, L1, Rs, Rs, P, P) :-
  arg_num(TestArg, Rs, TestVal),
  arg_num(DistArg, Rs, DistVal),
  TestVal #\= 0,
  L1 #= L + DistVal.
execute_instruction(tgl(Arg), L, L1, Rs, Rs, P, P1) :-
  arg_num(Arg, Rs, Dist),
  ToggleL #= L + Dist,
  get_assoc(ToggleL, P, I, P1, I1),
  toggle_instruction(I, I1),
  L1 #= L + 1.
execute_instruction(tgl(Arg), L, L1, Rs, Rs, P, P) :-
  arg_num(Arg, Rs, Dist),
  ToggleL #= L + Dist,
  \+ get_assoc(ToggleL, P, _),
  L1 #= L + 1.

%% arg_num(+Arg, +Registers, -Value) is det
%  Extracts the Value of Arg, either directly in the case of a v functor, or
%  through a Registers lookup in the case of an r functor.
arg_num(v(V), _, V).
arg_num(r(R), Rs, Rs.R).

%% toggle_instruction(+I, -T) is det
%% toggle_instruction(-I, +T) is multi
%  T is instruction I after the application of a toggle command.
toggle_instruction(inc(X), dec(X)).
toggle_instruction(dec(X), inc(X)).
toggle_instruction(tgl(X), inc(X)).
toggle_instruction(jnz(X, Y), cpy(X, Y)).
toggle_instruction(cpy(X, Y), jnz(X, Y)).

%% factorial(?N, ?R) is det
%  R is N factorial.
factorial(0, 1).
factorial(N, R) :-
  N #> 0,
  N #=< R,    % Set bound for N to prevent infinite CLP search in (-, +) mode
  N1 #= N - 1,
  factorial(N1, R1),
  R #= N * R1.

% Input grammar
instruction_dcg(cpy(V, Dest)) --> "cpy ", arg_dcg(V), " ", arg_dcg(Dest).
instruction_dcg(inc(R)) --> "inc ", arg_dcg(R).
instruction_dcg(dec(R)) --> "dec ", arg_dcg(R).
instruction_dcg(jnz(T, Dist)) --> "jnz ", arg_dcg(T), " ", arg_dcg(Dist).
instruction_dcg(tgl(Dist)) --> "tgl ", arg_dcg(Dist).

arg_dcg(v(V)) --> integer(V).
arg_dcg(r(R)) --> nonblanks(Cs), { atom_codes(R, Cs) }.

instructions_dcg([I|Is]) --> instruction_dcg(I), blanks, instructions_dcg(Is).
instructions_dcg([]) --> [].
