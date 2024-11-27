move(state([X | L], C, R), state(L, [X | C], R)).
move(state([X | L], C, R), state(L, C, [X | R])).
move(state(L, [X | C], R), state(L, C, [X | R])).

prolong([X | T], [Y, X | T]) :- 
    move(X, Y), 
    not(member(Y, T)).

dfs(Start, End, Res) :- dfsx([Start], End, Res).

dfsx([End | T], End, [End | T]).
dfsx(P, E, Res) :-
    prolong(P, P1),
    dfsx(P1, E, Res).

bfs(Start, End, Res) :- bfsx([[Start]], End, Res).

bfsx([[End | T] | _], End, [End | T]).
bfsx([Path | QT], End, Res) :-
    findall(X, prolong(Path, X), Paths),
    append(QT, Paths, OQ),
    bfsx(OQ, End, Res).
    

generate_w(0, []).
generate_w(Len, [w, b | T]) :-
    Len > 0,
    Len1 is Len - 2,
    length(T, Len1),
    generate_w(Len1, T).

generate_b(0, []).
generate_b(Len, [b, w | T]) :-
    Len > 0,
    Len1 is Len - 2,
    length(T, Len1),
    generate_b(Len1, T).

generate(L, R) :- generate_b(L, R); generate_w(L, R).

solve(Left, Res) :- length(Left, Len), generate(Len, Right), bfs(state(Left, [], []), state([], [], Right), Res).
