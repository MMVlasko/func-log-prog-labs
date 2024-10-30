% проверка принадлежности элемента к списку
my_member(H, [H | _]).
my_member(H, [_ | T]) :- 
    my_member(H, T).

% преобразование списка элементов вида grade(Subj, Grade) в список элементов Grade
structers_to_grades([], []).
structers_to_grades([grade(_, X) | T], [X | L]) :- 
    structers_to_grades(T, L). 

% получение списка оценок данного студента данной группы
% с учётом возможного наличия студентов с одинаковыми фамилиями в разных группах
grades(Group, Student, Res) :- 
    student(Group, Student, Grades), 
    structers_to_grades(Grades, Res).  

% подсчёт количества элементов числового списка и их суммы
sum_and_count([H], H, 1).
sum_and_count([H | T], Sum, Count) :- 
    sum_and_count(T, S, C), 
    Sum is S + H, 
    Count is C + 1.

% поиск среднего арифметического
middle(L, X) :- 
    sum_and_count(L, S, C), 
    X is S / C.

% проверка отсутствия несданных экзаменов у данного студента данной группы
pass(Group, Student, Pass) :- 
    student(Group, Student, Grades), 
    structers_to_grades(Grades, L), 
    (my_member(2, L) -> Pass = false ; Pass = true).

% получение среднего арифметического оценок и информации о наличии 
% несданных экзаменов данного студента данной группы
student_info(Group, Student, Middle, Pass) :- 
    pass(Group, Student, Pass),
    grades(Group, Student, L),
    middle(L, Middle).

% получение среднего балла и факта о сдаче для каждого студента
students_info(Res) :- 
    findall(student_info(Group, Student, Middle, Pass), student_info(Group, Student, Middle, Pass), Res). % 1


% получение количества вхождений данного элемента в данный список
count(_, [], 0).
count(Elem, [Elem | T], Res) :-
     count(Elem, T, TRes), 
     Res is TRes + 1, !.
count(Elem, [_ | T], Res) :- 
    count(Elem, T, Res), !.

% получение оценки Grade по данному предмету Subj из списка элементов вида grade(Subj, Grade)
get_grade(Subj, [grade(Subj, Grade) | _], Grade).
get_grade(Subj, [_ | T], Grade) :- 
    get_grade(Subj, T, Grade).

% получение списка оценок всех студентов по данному предмету
grades_by_subject(Subj, RGrades) :- 
    findall(Grade, (student(_, _, Grades), get_grade(Subj, Grades, Grade)), RGrades).

% получение количество несдавших по данному предмету
not_pass_count(SubjName, Count) :- 
    subject(Subj, SubjName), 
    grades_by_subject(Subj, Grades), 
    count(2, Grades, Count).

% получение количества несдавших для всех предметов
subjects_passing_info(Res) :-
    findall(not_pass_count(SubjName, Count), not_pass_count(SubjName, Count), Res). % 2


% проверка того, что данный элемент больше или равен любого элемента данного списка
more_than_everybody_in_list(_, []).
more_than_everybody_in_list(X, [H | T]) :- 
    X >= H, 
    more_than_everybody_in_list(X, T).

% получение максимального элемента данного списка
max_of_list(L, X) :- 
    my_member(X, L), 
    more_than_everybody_in_list(X, L).

% получение средней оценки у данного студента данной группы
get_middle(Group, Student, Middle) :- 
    student(Group, Student, SGrades), 
    structers_to_grades(SGrades, Grades), 
    middle(Grades, Middle).

% получение списка средних оценок студентов данной группы
get_middles(Group, Middles) :- 
    findall(Middle, get_middle(Group, _, Middle), Middles).

% получение очередного студента с максимальной средней оценкой у данной группы
max_middle_student(Group, Student) :- 
    get_middles(Group, Middles), 
    max_of_list(Middles, Max), 
    get_middle(Group, Student, Max).

% получение списка всех студентов с максимальной средней оценкой у данной группы
max_middle(Group, Students) :- 
    setof(Student, max_middle_student(Group, Student), Students).

% получение списков студентов с максимальным средним баллом для каждой группы
max_middles(Res) :-
    findall(max_middle(Group, Students), max_middle(Group, Students), Res). % 3