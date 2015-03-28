
/* Problem 1 */

put(K,V,[],[[K,V]]).
put(K,V,[[K,_]|D],[[K,V]|D]).
put(K,V,[[K0,V0]|D],[[K0,V0]|D0]) :- \+(K=K0),put(K,V,D,D0).

get(K,[[K,V]|_],V).
get(K,[[_,_]|D],V) :- get(K,D,V).


/* Problem 2 */

subseq([], []).
subseq([H|T1], [H|T2]) :- subseq(T1, T2).
subseq(L, [_|T2]) :- subseq(L, T2).


/* Problem 3 */

isSafe(R) :- permutation([1,2,3,4], R).

sudoku(Initial,Final) :-
	Initial = [R1, R2, R3, R4],

	% the rows cover all numbers
	isSafe(R1), isSafe(R2), isSafe(R3), isSafe(R4),
	
	R1 = [A1,A2,A3,A4],
	R2 = [B1,B2,B3,B4],
	R3 = [C1,C2,C3,C4],
	R4 = [D1,D2,D3,D4],

	% the columns cover all numbers
	isSafe([A1,B1,C1,D1]), isSafe([A2,B2,C2,D2]),
	isSafe([A3,B3,C3,D3]), isSafe([A4,B4,C4,D4]),

	% the 2x2 squares cover all numbers
	isSafe([A1,A2,B1,B2]), isSafe([A3,A4,B3,B4]),
	isSafe([C1,C2,D1,D2]), isSafe([C3,C4,D3,D4]),
	
	Final = Initial.

/* Problem 4 */	

digits([]).
digits([X|Xs]) :- member(X,[0,1,2,3,4,5,6,7,8,9]), digits(Xs).

nonzeroFirstDigit([]).
nonzeroFirstDigit([X|_]) :- \+(X=0).

evalDigits([], 1, 0).
evalDigits([D|Ds], NewM, NewRes) :- evalDigits(Ds, M, Res), NewM is M * 10, NewRes is D * M + Res. 

evalDigits(Ds, Res) :- evalDigits(Ds, _, Res).

verbalArithmetic(Letters, Op1, Op2, Sum) :-
  fd_all_different(Letters), digits(Letters),
  nonzeroFirstDigit(Op1), nonzeroFirstDigit(Op2), nonzeroFirstDigit(Sum),
  evalDigits(Op1, X1), evalDigits(Op2, X2), evalDigits(Sum, X3),
  X1 + X2 =:= X3.
  

/* Problem 5 */

isLegal(_, []).
isLegal(N, [H|_]) :- N < H.
    
move([[H|T], P2, P3], to(peg1, peg2), [T, [H|P2], P3]) :- isLegal(H, P2).
move([[H|T], P2, P3], to(peg1, peg3), [T, P2, [H|P3]]) :- isLegal(H, P3).
move([P1, [H|T], P3], to(peg2, peg1), [[H|P1], T, P3]) :- isLegal(H, P1).
move([P1, [H|T], P3], to(peg2, peg3), [P1, T, [H|P3]]) :- isLegal(H, P3).
move([P1, P2, [H|T]], to(peg3, peg1), [[H|P1], P2, T]) :- isLegal(H, P1).
move([P1, P2, [H|T]], to(peg3, peg2), [P1, [H|P2], T]) :- isLegal(H, P2).

towerOfHanoi(Goal, Goal, []).
towerOfHanoi(Curr, Goal, [Action|Rest]) :-
	move(Curr, Action, Curr2), towerOfHanoi(Curr2, Goal, Rest).

	
