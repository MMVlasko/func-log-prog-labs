my_length([], 0).
my_length([_ | T], Length) :- 
    my_length(T, TLength), 
    Length is TLength + 1.

my_remove(H, [H | T], T).
my_remove(X, [H | T1], [H | T2]) :- 
    my_remove(X, T1, T2).

my_permute([], []).
my_permute(L, [H | Perm]) :- 
    my_remove(H, L, L1), 
    my_permute(L1, Perm).

my_append([], X, X).
my_append([H | T1], Y, [H | T2]) :- 
    my_append(T1, Y, T2).

my_sublist([], _).
my_sublist([H1 | T1], [H2 | T2]) :- 
    H1 == H2, 
    my_sublist(T1, T2); 
    my_sublist([H1 | T1], T2).

my_member(H, [H | _]).
my_member(H, [_ | T]) :- 
    my_member(H, T).


% удаление последних трёх элементов без использования стандартных предикатов
rm_last_three([], []).
rm_last_three([_], []) :- !.
rm_last_three([_, _], []) :- !.
rm_last_three([_, _, _], []) :- !.
rm_last_three([H1 | T1], [H1 | T2]) :- 
    rm_last_three(T1, T2).

% удаление последних трёх элементов с использованием стандартных предикатов
rm_last_three_std(X, []) :- 
    length(X, L), 
    L < 4, !.
rm_last_three_std([H1 | T1], [H1 | T2]) :- 
    rm_last_three_std(T1, T2).


% подсчёт количества элементов числового списка и их суммы
sum_and_count([H], H, 1) :- !.
sum_and_count([H | T], Sum, Count) :- 
    sum_and_count(T, S, C), 
    Sum is S + H, 
    Count is C + 1.

% поиск среднего арифметического без стандартных предикатов
middle(L, X) :- 
    sum_and_count(L, S, C), 
    X is S / C.

% поиск среднего арифметического с стандартными предикатами
middle_std(X, M) :- 
    sumlist(X, S), 
    length(X, L), 
    M is S / L.