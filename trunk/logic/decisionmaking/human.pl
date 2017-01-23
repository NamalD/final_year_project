% % % % % %
% Human decision making
% % % % % %

user_decide(R, T, A) :-
	format('~w enter action: ',[R]),
	read_term(A, []),
  legal(R, A, T).
