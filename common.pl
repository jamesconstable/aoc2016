:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(readutil)).

read_input(Grammar) :- phrase_from_stream(Grammar, user_input).

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

%% count(:Pred, +Xs, -N) is det
%  N is the number of elements in Xs that satisfy Pred.
count(_, [], 0).
count(Pred, [X|Xs], N1) :- call(Pred, X), !, count(Pred, Xs, N), N1 #= N + 1.
count(Pred, [_|Xs], N) :- count(Pred, Xs, N).

%% rotate_list(?Distance, ?List, ?Rotated)
%  Rotated is the forwards rotation of List by Distance. For example,
%  rotate_list(1, [a, b, c], X) -> X = [c, a, b]. Can be used in any mode,
%  though CLP(FD) membership constraints may be required if Distance is unbound.
rotate_list(Distance, List, Rotated) :-
  length(List, L),
  N #= Distance mod L,
  split_at(N, List, A, B),
  append(B, A, Rotated), !.

%% split_at(?Index, ?List, ?Init, ?Tail)
%  List is the concatenation of Init and Tail, where Init has length Index.
split_at(0, Xs, [], Xs).
split_at(N, [X|Xs], [X|A], B) :- N #> 0, N1 #= N-1, split_at(N1, Xs, A, B).

%% rows_cols(+Rows, -Cols) is det
%% rows_cols(-Rows, +Cols) is det
%  Rows is the transpose of Cols, where both are lists of lists.
rows_cols(Rows, []) :- maplist(=([]), Rows), !.
rows_cols(Rows, [Col|Cols]) :-
  maplist([[C|Cs], C, Cs]>>true, Rows, Col, Rows1),
  rows_cols(Rows1, Cols).

%% get_default_assoc(+Key, +Default, +Assoc, -Value) is det
%  Value is the mapping of Key in Assoc, or Default if Assoc does not have a
%  mapping for Key.
get_default_assoc(K, _, Assoc, V) :- get_assoc(K, Assoc, V).
get_default_assoc(K, Default, Assoc, Default) :- \+ get_assoc(K, Assoc, _).

%% set_nth0(?N, ?Xs, ?V, ?Xs1)
%  Xs1 is Xs, but with the item at index N (zero-based) set to V.
set_nth0(N, List, Elem, List1) :-
  nth0(N, List, _, ListWithoutN),
  nth0(N, List1, Elem, ListWithoutN).

%% set_nth1(?N, ?Xs, ?V, ?Xs1)
%  Xs1 is Xs, but with the item at index N (one-based) set to V.
set_nth1(N, List, Elem, List1) :-
  nth1(N, List, _, ListWithoutN),
  nth1(N, List1, Elem, ListWithoutN).

%% yes(@Arg1, ...) is det
%  Always succeeds.
yes(_).
yes(_, _).
yes(_, _, _).

%% no(@Arg1, ...) is det
%  Always fails.
no(_) :- false.
no(_, _) :- false.
no(_, _, _) :- false.

%% bfs(+Init, :NextState, :IsGoal, :Canonical, -Final, -Cost)
%  Performs a breadth-first search given the following parameters:
%    * Init - the initial state
%    * NextState - a predicate that generates successor states on backtracking
%    * IsGoal - a predicate that takes a state and checks whether it is a valid
%        goal state
%    * Canonical - a predicate that converts a state to a canonical
%        representation (e.g. a hash function), for use in avoiding previously
%        visited states. Useful for pruning the search space if the problem has
%        many equivalent but non-identical states. If not required, (=) can be
%        used as an identity predicate.
%    * Final - the first goal state encountered. Returns successive, higher cost
%        goals on backtracking.
%    * Cost - the number of steps taken to reach Final
%  Fails if search space is fully expanded without encountering a goal state.
bfs(Init, NextState, IsGoal, Canonical, Final, Cost) :-
  empty_assoc(Seen),
  bfs([0-Init|H], H, Seen, NextState, IsGoal, Canonical, Final, Cost).

bfs(Q, _, _, _, _, _, _, _) :-
  var(Q), !, fail.
bfs([Step-State|_], _, Seen, _, IsGoal, Canonical, State, Step) :-
  \+ is_seen(State, Canonical, Seen),
  call(IsGoal, State).
bfs([_-State|Q], Q1, Seen, NextState, IsGoal, Canonical, Final, Cost) :-
  is_seen(State, Canonical, Seen),
  !,    % Green cut; remove choice point to enable tail recursion
  bfs(Q, Q1, Seen, NextState, IsGoal, Canonical, Final, Cost).
bfs([Step-State|Q], Q1, Seen, NextState, IsGoal, Canonical, Final, Cost) :-
  \+ is_seen(State, Canonical, Seen),
  call(Canonical, State, StateC),
  put_assoc(StateC, Seen, _, Seen1),
  findall(Next, call(NextState, Step, State, Next), NextStates),
  Step1 #= Step + 1,
  maplist({Step1}/[S, Step1-S]>>true, NextStates, NextStates1),
  exclude(
    {Canonical, Seen1}/[Cost-S]>>is_seen(S, Canonical, Seen1),
    NextStates1, NextStates2),
  append(NextStates2, Q2, Q1),
  bfs(Q, Q2, Seen1, NextState, IsGoal, Canonical, Final, Cost).

%% is_seen(+State, :Canonical, +Seen) is semidet
%  Succeeds if the canonical form of State is in Seen.
is_seen(State, Canonical, Seen) :-
  call(Canonical, State, StateC),
  get_assoc(StateC, Seen, _).

