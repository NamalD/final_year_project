:- use_module(library(lists)).

%%% GAME INFORMATION
% Get list of actions
actions(L) :-
	findall(A, (input(_, A)), L).

% Get list of roles
roles(L) :-
	findall(R, (role(R)), L).



% Get all fluents that hold at time T
get_state(F, T) :-
	base(F),
	holds_at(F, T).

% Get all legal moves for role R at time T
get_legal_moves(R, T, L) :-
	findall(A, legal(R, A, T), L).

% Print goal value for roles
print_goals([], _).
print_goals([Head|Rest], T) :-
	goal(Head, G, T),
	format('~w: ~w~n', [Head, G]),
	print_goals(Rest, T).

% Allowed player types
player_type(human).
player_type(random).

%%% TRANSFORMATIONS
% Allow noop action (no transformation applied)
transform_noop(R, noop, T) :-
	% Check if noop action is given
	role(R),
	T > -1.

% Initiates assertions
transform_initiate([], _, _, _).
transform_initiate([Head|Rest], R, A, T) :-
	initiate_time(R, A, Head, Ts),
	T1 is T + Ts,
	assert(initiates(A, Head, T1)),
	transform_initiate(Rest, R, A, T).

% Terminates assertions
transform_terminate([], _, _, _).
transform_terminate([Head|Rest], R, A, T) :-
	terminate_time(R, A, Head, Ts),
	T1 is T + Ts,
	assert(terminates(A, Head, T1)),
	transform_terminate(Rest, R, A, T).

% Apply all applicable transformation assertions
apply_transformations(R, A, T) :-
	% List of all initiated fluents
	findall(Fluent, initiate_time(R, A, Fluent, _), IList),
	% List of all terminated fluents
	findall(Fluent, terminate_time(R, A, Fluent, _), TList),

	% Apply assertions for transitions on fluents
	transform_initiate(IList, R, A, T),
	transform_terminate(TList, R, A, T).

%%% GAME ACTION HANDLING
% Get an action from non-human players
handle_moves([], _).
handle_moves([R|Rest], T) :-
	role_player(R, P),
	P \= human,
	ask_player(P, R, T),
	handle_moves(Rest, T).

% Random player moves
ask_player(random, R, T) :-
	rand_decide(R, T, A),
	handle_transforms(R, A, T).

% Common transformations
handle_transforms(R, A, T) :-
	% Assert that an action occurs at this time
	assert(happens(A, T)),

	% Normal transformations for non-noop moves
	A \= noop ->
		apply_transformations(R, A, T);
	transform_noop(R, noop, T).

%%% GAME LOOP (TESTING ONLY)
start :-
	% x = human, o = random player
	assign_player(random, xplayer),
	assign_player(random, oplayer),

	game(0).

% T = current time step
% R = game role(s)
game(T) :-
	% Print current state
	findall(State_Fluent, get_state(State_Fluent, T), L),
	format('~nCurrent state: ~w~n~n', [L]),
	
	% If terminal, then stop
	terminal(T) *-> (
		roles(R),
		format('Game has ended. Goal values for each player:~n'),
		print_goals(R, T)
	);
	(	% Otherwise, progress game

		roles(R),
		perform_moves(R, T),

		T1 is T + 1,	% Advance time
		game(T1)
	).
