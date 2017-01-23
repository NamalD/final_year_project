:- use_module(library(lists)).

:- dynamic 
	initiates/3,
	terminates/3,
	happens/2.
%%% TEMPORAL LOGIC PREDICATES

% HoldAt
holds_at(F, T) :-
	% Fluent holds if it was an initial fluent 
	% and hasn't been terminated
	init(F),
	\+ clipped(0, F, T).
	
	
holds_at(F, T2) :-
	initiates(A, F, T1),
	happens(A, T1),
	T1 < T2,
	\+ clipped(T1, F, T2).
	
% Clipped
clipped(T1, F, T2) :-
	happens(A, T),
	terminates(A, F, T),
	T >= T1,
	T2 > T.