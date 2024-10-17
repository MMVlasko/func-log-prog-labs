my_length([], 0).
my_length([_ | T], Length) :- my_length(T, TLength), Length is TLength + 1.

remove([H | T], H, T).
remove([H | T1], X, [H | T2]) :- remove(T1, X, T2).

permute([], []).
permute(L, [H | Perm]) :- remove(L, H, L1), permute(L1, Perm).

my_append([], X, X).
my_append([H, T1], Y, [H | T2]) :- my_append(T1, Y, T2).

my_sublist([], _).
my_sublist([H1 | T1], [H2 | T2]) :- H1 == H2, my_sublist(T1, T2); my_sublist([H1 | T1], T2).

my_member(H, [H | _]).
my_member(H, [_ | T]) :- my_member(H, T).


% удаление последних трёх элементов без использования стандартных предикатов
rm_last_three_elems([_], []).
rm_last_three_elems([_, _], []).
rm_last_three_elems([_, _, _], []).
rm_last_three_elems([H1 | T1], [H1 | T2]) :- rm_last_three_elems(T1, T2).

% удаление последних трёх элементов с использованием стандартных предикатов
rm_last_three_elems_with_std_preds(X, []) :- my_length(X, L), L < 4.
rm_last_three_elems_with_std_preds([H1 | T1], [H1 | T2]) :- rm_last_three_elems_with_std_preds(T1, T2).


% подсчёт количества элементов числового списка и их суммы
sum_and_count([H], H, 1).
sum_and_count([H | T], Sum, Count) :- sum_and_count(T, S, C), Sum is S + H, Count is C + 1.

% поиск среднего арифметического без стандартных предикатов
middle(L, X) :- sum_and_count(L, S, C), X is S / C.

% поиск среднего арифметического с стандартными предикатами
middle_with_std_preds(X, M) :- sumlist(X, S), length(X, L), M is S / L.