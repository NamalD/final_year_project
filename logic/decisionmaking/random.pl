% % % % % %
% Random decision making
% Selects a random legal move
% % % % % %

%%% DECIDE
% Return a random legal action
rand_decide(R, T, A) :-
  get_legal_moves(R, T, L),
  random_member(A, L).
