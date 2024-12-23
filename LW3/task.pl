% возмлжные изменения состояний
move(state([X | L], C, R), state(L, [X | C], R)).

move(state([X | L], C, []), state(L, C, [X])).
move(state(L, [X | C], []), state(L, C, [X])).

% для перехода на правую сторону сразу предполагается только чередование
move(state([X | L], C, [Y | R]), state(L, C, [X, Y | R])) :- not(X = Y).
move(state(L, [X | C], [Y | R]), state(L, C, [X, Y | R])) :- not(X = Y).


% поиск продолжений пути
prolong([X | T], [Y, X | T]) :- 
    move(X, Y), 
    not(member(Y, T)).


% обёртка над поиском в глубину, определяющая данное кол-во вагонов
dfs(state(L, C, R), Res, All) :-
    dfsx([state(L, C, R)], Res, All).

% поиск в глубину заканчивается, когда все вагоны оказываются справа
% при поиске кратчайшего - остановка после нахождения первого решения длины 3 * n
dfsx([state([], [], A) | T], [state([], [], A) | T], All) :-
    (All == all; length([state([], [], A) | T], Ln), length(A, Len), Ln is 3 * (Len / 2), !).
dfsx(P, Res, All) :-
    prolong(P, P1),
    dfsx(P1, Res, All).


% обёртка над поиском в ширину, определяющая данное кол-во вагонов
bfs(state(L, C, R), Res, All) :-
    bfsx([[state(L, C, R)]], Res, All).

% поиск в ширину заканчивается, когда все вагоны оказываются справа
% при поиске кратчайшего - остановка после нахождения первого решения
bfsx([[state([], [], A) | T] | _], [state([], [], A) | T], All) :-
    (All == all -> true; !).
bfsx([Path | QT], Res, All) :-
    findall(X, prolong(Path, X), Paths),
    append(QT, Paths, OQ), !,
    bfsx(OQ, Res, All).
bfsx([_ | QT], Res, All) :-
    bfsx(QT, Res, All).


% обёртка над поиском в глубину для данного предела глубины
dfs_id(state(L, C, R), Res, DL, All) :-
    dfsx_id([state(L, C, R)], Res, DL, All).

% поиск в глубину для данного предела глубины
% при поиске кратчайшего - остановка после нахождения первого решения
dfsx_id([state([], [], A) | T], [state([], [], A) | T], _, All) :-
    (All == all -> true; !).
dfsx_id(Path, Res, DL, All) :-
    DL > 0,
    prolong(Path, NewPath),
    NewDL is DL - 1,
    dfsx_id(NewPath, Res, NewDL, All).

% генератор натуральных чисел
nature(1).
nature(N) :-
    nature(M),
    N is M + 1.

% поиск в глубину с итеративным погружением
srch_id(Start, Res, DL, All) :-
    nature(DLi),
    (DLi < DL -> true; !),
    dfs_id(Start, Res, DLi, All),
    (All == all -> true; !).


% Оценка числа нарушений чередования на правой стороне
heuristic(state(_, _, R), H) :-
    findall(1, (nth1(I, R, X), nth1(J, R, Y), abs(I - J) =:= 1, X = Y), Violations),
    length(Violations, H).


% A* для сортировочного узла
solve_astar(InitialState, Path, All) :-
    heuristic(InitialState, H),
    astar([node(InitialState, [], 0, H)], Path, All).

% Основная логика A*
astar([node(state([], [], R), Path, _, _) | _], [state([], [], R) | Path], All) :-
    (All == all -> true; !).

astar([node(State, Path, G, _) | RestQueue], FinalPath, All) :-
    findall(node(NextState, [State | Path], GNew, FNew),
            (move(State, NextState), % Переходы между состояниями
             \+ member(NextState, Path), % Избегаем циклов
             GNew is G + 1, % Считаем фактическую стоимость пути
             heuristic(NextState, H),
             FNew is GNew + H), % Общая стоимость
            Successors),
    append(RestQueue, Successors, NewQueue),
    sort(4, @=<, NewQueue, SortedQueue), % Сортируем по F
    astar(SortedQueue, FinalPath, All).


solve_greedy(InitialState, Path, All) :-
    heuristic(InitialState, H),
    greedy([node(InitialState, [], H)], Path, All).

% Основная логика жадного поиска
greedy([node(state([], [], R), Path, _) | _], [state([], [], R) | Path], All) :-
    (All == all -> true; !).

greedy([node(State, Path, _) | RestQueue], FinalPath, All) :-
    findall(node(NextState, [State | Path], H),
            (move(State, NextState),
             \+ member(NextState, Path),
             heuristic(NextState, H)),
            Successors),
    append(RestQueue, Successors, NewQueue),
    sort(3, @=<, NewQueue, SortedQueue), % Сортируем по H
    greedy(SortedQueue, FinalPath, All).


% проверка списка вагонов на корректность
check_wb([]).
check_wb([H | T]) :-
    (H == w; H == b), 
    check_wb(T).

% предикат решения, принимающий вид левого пути и метод 
% поиска или предел для итеративного поиска
solve(Left, Res, MetOrDL, All) :- 
    length(Left, L), 0 is L mod 2, check_wb(Left),
    (
        MetOrDL == d -> dfs(state(Left, [], []), Res, All); 
        (
            MetOrDL == w -> bfs(state(Left, [], []), Res, All);
            (
                MetOrDL == ast -> solve_astar(state(Left, [], []), Res, All);
                (
                    MetOrDL == greedy -> solve_greedy(state(Left, [], []), Res, All);
                    srch_id(state(Left, [], []), Res, MetOrDL, All)
                )
            )
        )
    ).


% вывод информации о данном состоянии сортировочного узла
write_state(Step, state(L, C, R)) :-
    write(Step),
    write(': L = '),
    write(L),
    write('; C = '),
    write(C),
    write('; R = '),
    write(R),
    write('.'), nl.

% вывод информации о пронумерованной последовательности 
% состояний, состовляющих найденный путь
write_steps([], _).
write_steps(L, Step) :-
    append(H, [Now], L),
    write_state(Step, Now),
    NewStep is Step + 1,
    write_steps(H, NewStep).

% вывод всех решений из списка
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

% основной предикат решения
solve(Left, Met, All) :-
    setof(X, solve(Left, X, Met, All), Res),
    length(Res, L), L > 0,
    write_solutions(Res), !.


% генератор последовательности из w и b данной длины
generate_wb([], 0) :- !.
generate_wb([A, B | T], N) :-
    A = w, B = b, 
    N1 is N - 2, 
    generate_wb(T, N1).

% хронометрировние поисков разными методами для данного числа вагонов
chrono(Length, DL, All) :-
    0 is Length mod 2,
    generate_wb(Left1, Length),
    generate_wb(Left2, Length),
    generate_wb(Left3, Length),

    write('Поиск в глубину (все пути): '),
    get_time(Start),
    findall(R1, solve(Left1, R1, d, All), _),
    get_time(End),
    Re is End - Start,
    write(Re), nl,

    write('Поиск в ширину (все пути): '),
    get_time(Start1),
    findall(R1, solve(Left2, R2, w, All), _),
    get_time(End1),
    Re1 is End1 - Start1,
    write(Re1), nl,

    write('Поиск в глубину с итеративным погружением (все пути): '),
    get_time(Start2),
    findall(R1, solve(Left3, R2, DL, All), _),
    get_time(End2),
    Re2 is End2 - Start2,
    write(Re2), nl.