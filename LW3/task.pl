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


ws(state(L, C, R), Res) :- 
    length(L, Len),
    wsx([[state(L, C, R)]], Res, Len).

wsx([[state([], [], A) | T] | _], [state([], [], A) | T], Len) :-
    length(A, Len).
wsx([Path | QT], Res, Len) :-
    findall(X, prolong(Path, X), Paths),
    append(QT, Paths, OQ), !,
    wsx(OQ, Res, Len).
wsx([_ | QT], Res, Len) :-
    wsx(QT, Res, Len).


dfs_id(state(L, C, R), Res, DL) :-
    length(L, Len),
    dfsx_id([state(L, C, R)], Res, Len, DL).

dfsx_id([state([], [], A) | T], [state([], [], A) | T], Len, DL) :-
    length(A, Len), DL == 0.
dfsx_id(Path, Res, Len, DL) :-
    DL > 0,
    prolong(Path, NewPath),
    NewDL is DL - 1,
    dfsx_id(NewPath, Res, Len, NewDL).

nature(1).
nature(N) :-
    nature(M),
    N is M + 1.

srch_id(Start, Res, MLen) :-
    nature(DL),
    (DL < MLen + 1 -> true; !),
    dfs_id(Start, Res, DL).


solve(Left, Res, Met) :- 
    (
        Met == d -> dfs(state(Left, [], []), Res); 
        (
            Met == w -> ws(state(Left, [], []), Res);
            length(Left, Len), MLen is 3 * Len + 1, srch_id(state(Left, [], []), Res, MLen)
        )
    ).


write_state(Step, state(L, C, R)) :-
    write(Step),
    write(': L = '),
    write(L),
    write('; C = '),
    write(C),
    write('; R = '),
    write(R),
    write('.'), nl.

write_steps([], _).
write_steps(L, Step) :-
    append(H, [Now], L),
    write_state(Step, Now),
    NewStep is Step + 1,
    write_steps(H, NewStep).

write_solutions([]).
write_solutions([H | T]) :-
    write_steps(H, 1),
    (
        length(T, 0) -> true;
        write('-----------'), nl,
        write('OR'), nl,
        write('-----------'), nl,
        write_solutions(T)
    ).


solve(Left, Met) :-
    findall(X, solve(Left, X, Met), Res),
    write_solutions(Res), !.

generate_wb([], 0) :- !.
generate_wb([A, B | T], N) :-
    A = w, B = b, N1 is N - 2, generate_wb(T, N1).

chrono(Length) :-
    0 is Length mod 2,
    generate_wb(Left1, Length),
    generate_wb(Left2, Length),
    generate_wb(Left3, Length),
    write('Поиск в глубину:'), nl,
    time(findall(R1, solve(Left1, R1, d), _)),
    write('Поиск в ширину:'), nl,
    time(findall(R2, solve(Left2, R2, w), _)),
    write('Поиск в глубину с итеративным погружением:'), nl,
    time(findall(R2, solve(Left3, R2, x), _)).