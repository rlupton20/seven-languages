valid_board([]).
valid_board([Head|Tail]) :- member(Head, [1,2,3,4,5,6,7,8]), valid_board(Tail).

diagonals1(Queens, Diags) :- diags_ctr_1(Queens,Diags,1).
diags_ctr_1([],[],9).
diags_ctr_1([Column|Tail], [Diag|OtherDiags], Row) :- Diag is Column + Row,
						      NewRow is Row + 1,
						      diags_ctr_1(Tail,OtherDiags,NewRow).

diagonals2(Queens, Diags) :- diags_ctr_2(Queens,Diags,1).
diags_ctr_2([],[],9).
diags_ctr_2([Column|Tail], [Diag|OtherDiags], Row) :- Diag is Column - Row,
						      NewRow is Row + 1,
						      diags_ctr_2(Tail,OtherDiags,NewRow).

eight_queens(Queens) :- Queens = [_,_,_,_,_,_,_,_],
			valid_board(Queens),
			diagonals1(Queens, Diags1),
			diagonals2(Queens, Diags2),
			fd_all_different(Queens),
			fd_all_different(Diags1),
			fd_all_different(Diags2).
