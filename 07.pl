#! /usr/bin/env swipl

% Solver for Day 7 of the Advent of Code 2016
% Problem description: https://adventofcode.com/2016/day/7

:- initialization(main, main).
:- ensure_loaded('common.pl').

main(['1']) :- !, run_with(supports_tls).
main(['2']) :- !, run_with(supports_ssl).
main(_) :- writeln(user_error, 'Invalid part number. Must be 1 or 2.').

run_with(Pred) :- read_input(lines_dcg(IPs)), count(Pred, IPs, C), writeln(C).

%% supports_tls(+IP) is semidet
%  Succeeds if the tokenised IP supports TLS.
supports_tls(IP) :- supports_tls(IP, false).

supports_tls([], true).
supports_tls([s(T)|Ts], _) :- has_abba(T), !, supports_tls(Ts, true).
supports_tls([h(T)|_], _) :- has_abba(T), !, fail.
supports_tls([_|Ts], S) :- supports_tls(Ts, S).

has_abba(T) :- phrase(has_abba_dcg, T, _).
has_abba_dcg --> string(_), [A, B, B, A], { A \= B }.

%% supports_ssl(+IP) is semidet
%  Succeeds if the tokenised IP supports SSL.
supports_ssl(Ts) :-
  tokens_abas_babs(Ts, ABAs, BABs),
  member([A,B,A], ABAs),
  member([B,A,B], BABs), !.

%% tokens_abas_babs(+Tokens, -As, -Bs) is det
%  Given a list of IP Tokens, As are all ABAs (XYX patterns found within
%  supernet sequences), and Bs are all BABs (XYX patterns found within hypernet
%  sequences).
tokens_abas_babs([], [], []).
tokens_abas_babs([s(T)|Ts], As, Bs) :-
  phrase(extract_xyxs_dcg(As, As1), T), !, tokens_abas_babs(Ts, As1, Bs).
tokens_abas_babs([h(T)|Ts], As, Bs) :-
  phrase(extract_xyxs_dcg(Bs, Bs1), T), !, tokens_abas_babs(Ts, As, Bs1).

xyx_dcg([X, Y, X]), [Y, X] --> [X, Y, X], { X \= Y }.

extract_xyxs_dcg([X|Xs], H) --> string(_), xyx_dcg(X), extract_xyxs_dcg(Xs, H).
extract_xyxs_dcg(H, H) --> remainder(_).

% Input grammar. Tokenises input file into a list of tokenised IPs, where the
% tokens are either s(X) for supernet sequences or h(X) for hypernet sequences.
lines_dcg([])     --> [].
lines_dcg([L|Ls]) --> ip_dcg(L), blanks, lines_dcg(Ls).

ip_dcg([s(S), h(H)|R]) --> string_without("[\n", S), hypernet_dcg(H), ip_dcg(R).
ip_dcg([s(S)])         --> string_without("[\n", S).

hypernet_dcg(H) --> "[", string_without("]", H), "]".
