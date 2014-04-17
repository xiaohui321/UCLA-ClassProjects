/*******************************************************************************
 * Problem 1: Dictionary
 */
put(K,V,[],[[K,V]]).
put(K,V,[[K,_]|T],[[K,V]|T]).
put(K,V,[H|T],[H|D2]) :- H \= [K,_], put(K,V,T,D2).

get(K,[[K,V]|_],V).
get(K,[_|T],V) :- get(K,T,V).

/*******************************************************************************   
 * Problem 2: Subsequence
 */
subseq([],_).
subseq([H|XT],[H|YT]) :- subseq(XT,YT).
subseq([XH|XT],[YH|YT]) :- subseq([XH|XT],YT),\+ (XH = YH).

/*******************************************************************************
 * Problem 3: Sudoku
 */
checkFour(L) :- permutation([1,2,3,4],L).

sudoku([[A1,A2,A3,A4],[B1,B2,B3,B4],[C1,C2,C3,C4],[D1,D2,D3,D4]],
       [[A1,A2,A3,A4],[B1,B2,B3,B4],[C1,C2,C3,C4],[D1,D2,D3,D4]]) :-
        checkFour([A1,A2,A3,A4]),
        checkFour([B1,B2,B3,B4]),
        checkFour([C1,C2,C3,C4]),
        checkFour([D1,D2,D3,D4]),

        checkFour([A1,B1,C1,D1]),
        checkFour([A2,B2,C2,D2]),
        checkFour([A3,B3,C3,D3]),
        checkFour([A4,B4,C4,D4]),

        checkFour([A1,A2,B1,B2]),
        checkFour([A3,A4,B3,B4]),
        checkFour([C1,C2,D1,D2]),
        checkFour([C3,C4,D3,D4]).

/*******************************************************************************
 * Problem 4: Verbal Arithmetic
 */
getValue([],V,V).
getValue([H|T],X,V) :- Y is 10 * X + H, getValue(T,Y,V).

beDigits([]).
beDigits([H|T]) :- member(H,[0,1,2,3,4,5,6,7,8,9]), beDigits(T).

/*if W3 has a longer length than both W1 and W2, then the first digit in W3 is 1*/
checkFirstDigit([W1H1,W1H2|W1T],[W2H1,W2H2|W2T],[W3H1,W3H2|W3T]) :-
        length([W1H1,W1H2|W1T],L),length([W2H1,W2H2|W2T],L),
	length([W3H1,W3H2|W3T],L3),
	L3 > L, W3H1 = 1, W1H1 > W3H2, W2H1 > W3H2.

verbalArithmetic(WL,[W1H|W1T],[W2H|W2T],[W3H|W3T]) :-
        beDigits(WL),
        fd_all_different(WL),
	checkFirstDigit([W1H|W1T],[W2H|W2T],[W3H|W3T]),
        (\+ W1H=0),(\+ W2H=0),(\+ W2H=0),
        getValue([W1H|W1T],0,V1),getValue([W2H|W2T],0,V2),getValue([W3H|W3T],0,V3),
        V1 + V2 =:= V3.

/*******************************************************************************
 * Problem 5: Tower of Hanoi
 */

towerOfHanoi(G,G,[]).
towerOfHanoi(I,G,[H|T]) :- move(H,I,N), (\+ invalidState(N)), towerOfHanoi(N,G,T).

invalidState([[H1,H2|_],_,_]) :- H1 > H2.
invalidState([_,[H1,H2|_],_]) :- H1 > H2.
invalidState([_,_,[H1,H2|_]]) :- H1 > H2.

move(to(peg1,peg2),[[H|T],Y,Z],[T,[H|Y],Z]).
move(to(peg1,peg3),[[H|T],Y,Z],[T,Y,[H|Z]]).
move(to(peg2,peg1),[X,[H|T],Z],[[H|X],T,Z]).
move(to(peg2,peg3),[X,[H|T],Z],[X,T,[H|Z]]).
move(to(peg3,peg1),[X,Y,[H|T]],[[H|X],Y,T]).
move(to(peg3,peg2),[X,Y,[H|T]],[X,[H|Y],T]).
