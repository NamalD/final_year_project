:- module(xo, [
	role/1, base/1, input/2, input/1, init/1, legal/3,
	goal/3, terminal/1, initiate_time/4, terminate_time/4
	]).

%%% Player Roles
role(xplayer).
role(oplayer).

%%% Base predicates/fluents
base(cell(X, Y, M)) :-
	boardDim(X),
	boardDim(Y),
	symbol(M).

base(control(R)) :-
	role(R).

% Supporting facts
boardDim(1).
boardDim(2).
boardDim(3).

symbol(x).
symbol(o).
symbol(b).	% b = blank

%%% Allowed player input/actions
input(R, mark(X, Y)) :-
	role(R),
	boardDim(X),
	boardDim(Y).

input(noop).

%%% Initial base predicates
init(cell(1, 1, b)).
init(cell(1, 2, b)).
init(cell(1, 3, b)).
init(cell(2, 1, b)).
init(cell(2, 2, b)).
init(cell(2, 3, b)).
init(cell(3, 1, b)).
init(cell(3, 2, b)).
init(cell(3, 3, b)).

init(control(xplayer)).

%%% Legal moves

% Player may mark only on their turn, on empty cells
legal(R, mark(X, Y), T) :-
	T > -1,
	holds_at(control(R), T),
	holds_at(cell(X, Y, b), T).

% Player can submit noop if its not their turn
legal(R, noop, T) :-
	T > -1,
	role(R),
	\+ holds_at(control(R), T).

%%% Goal values

% Win if player has a line
goal(xplayer, 100, T) :-
	line(x, T),
	\+ line(o, T).

goal(oplayer, 100, T) :-
	line(o, T),
	\+ line(o, T).

% Lose if other player has a line
goal(xplayer, 0, T) :-
	line(o, T),
	\+ line(x, T).

goal(oplayer, 0, T) :-
	line(x, T),
	\+ line(o, T).

% Draw if no one has a line
goal(R, 50, T) :-
	role(R),
	symbol(M),
	\+ line(M, T).

%%% Terminal states

% The game is in a terminal state if a line of player symbols has been made
terminal(T) :-
	symbol(M),
	M \= b,
	line(M, T),!.

% Also terminal if there is no open cell
terminal(T) :- \+ freeCell(T).

% Supporting rules for goals and terminal states
line(M, T) :-
	boardDim(X),
	row(M, X, T).

line(M, T) :-
	boardDim(Y),
	column(M, Y, T).

line(M, T) :-
	diagonal(M, T).

row(M, X, T) :-
	holds_at(cell(X, 1, M), T),
	holds_at(cell(X, 2, M), T),
	holds_at(cell(X, 3, M), T).

column(M, Y, T) :-
	holds_at(cell(1, Y, M), T),
	holds_at(cell(2, Y, M), T),
	holds_at(cell(3, Y, M), T).

diagonal(M, T) :-
	holds_at(cell(1, 1, M), T),
	holds_at(cell(2, 2, M), T),
	holds_at(cell(3, 3, M), T).

diagonal(M, T) :-
	holds_at(cell(1, 3, M), T),
	holds_at(cell(2, 2, M), T),
	holds_at(cell(3, 1, M), T).

freeCell(T) :-
	boardDim(X),
	boardDim(Y),
	holds_at(cell(X, Y, b), T).

%%% State transitions achieved through actions
% initiate_time(R, A, F, T) where:
% A is action by R that initiates holding of fluent F,
% and T is the time required to pass before this takes effect
% eg T = 0 means it takes effect from now.

% Supress warnings for not grouping clauses together.
:- discontiguous
	initiate_time/4,
	terminate_time/4.

% Cell marked x when xplayer marks a location.
initiate_time(xplayer, mark(X, Y), cell(X, Y, x), 0) :-
	boardDim(X),
	boardDim(Y).

initiate_time(oplayer, mark(X, Y), cell(X, Y, o), 0) :-
	boardDim(X),
	boardDim(Y).

% terminate_time is the same but marks when a fluent terminates
terminate_time(R, mark(X, Y), cell(X, Y, b), 0) :-
	role(R),
	boardDim(X),
	boardDim(Y).

% Alternate control when player makes move
terminate_time(R, mark(X, Y), control(R), 0) :-
	role(R),
	boardDim(X),
	boardDim(Y).

initiate_time(xplayer, mark(X, Y), control(oplayer), 0) :-
	boardDim(X),
	boardDim(Y).

initiate_time(oplayer, mark(X, Y), control(xplayer), 0) :-
	boardDim(X),
	boardDim(Y).
