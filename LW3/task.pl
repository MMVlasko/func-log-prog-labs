move(state([X | L], C, R), state(L, [X | C], R)).

move(state([X | L], C, []), state(L, C, [X])).
move(state(L, [X | C], []), state(L, C, [X])).

move(state([X | L], C, [Y | R]), state(L, C, [X, Y | R])) :- not(X = Y).
move(state(L, [X | C], [Y | R]), state(L, C, [X, Y | R])) :- not(X = Y).

prolong([X | T], [Y, X | T]) :- 
    move(X, Y), 
    not(member(Y, T)).

dfs(state(L, C, R), Res) :- 
    length(L, Len),
    dfsx([state(L, C, R)], Res, Len).

dfsx([state([], [], A) | T], [state([], [], A) | T], Len) :- 
    length(A, Len).
dfsx(P, Res, Len) :-
    prolong(P, P1),
    dfsx(P1, Res, Len).

ws(Start, End, Res) :- 
    wsx([[Start]], End, Res).

wsx([[End | T] | _], End, [End | T]).
wsx([Path | QT], End, Res) :-
    findall(X, prolong(Path, X), Paths),
    append(QT, Paths, OQ),
    wsx(OQ, End, Res).

solve(Left, Res) :- dfs(state(Left, [], []), Res).
