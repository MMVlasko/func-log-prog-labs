subject('LP','Логическое программирование').
subject('MTH','Математический анализ').
subject('FP','Функциональное программирование').
subject('INF','Информатика').
subject('ENG','Английский язык').
subject('PSY','Психология').

student(102,'Petrov',[grade('LP',3),grade('MTH',4),grade('FP',4),grade('INF',4),grade('ENG',5),grade('PSY',4)]).
student(101,'Петровский',[grade('LP',5),grade('MTH',4),grade('FP',3),grade('INF',4),grade('ENG',2),grade('PSY',4)]).
student(104,'Иванов',[grade('LP',4),grade('MTH',5),grade('FP',5),grade('INF',4),grade('ENG',3),grade('PSY',3)]).
student(102,'Ивановский',[grade('LP',3),grade('MTH',4),grade('FP',3),grade('INF',3),grade('ENG',3),grade('PSY',5)]).
student(104,'Запорожцев',[grade('LP',3),grade('MTH',3),grade('FP',3),grade('INF',5),grade('ENG',5),grade('PSY',2)]).
student(101,'Сидоров',[grade('LP',5),grade('MTH',3),grade('FP',5),grade('INF',5),grade('ENG',4),grade('PSY',2)]).
student(103,'Сидоркин',[grade('LP',4),grade('MTH',4),grade('FP',2),grade('INF',3),grade('ENG',4),grade('PSY',3)]).
student(102,'Биткоинов',[grade('LP',4),grade('MTH',5),grade('FP',5),grade('INF',3),grade('ENG',3),grade('PSY',4)]).
student(103,'Эфиркина',[grade('LP',4),grade('MTH',5),grade('FP',3),grade('INF',3),grade('ENG',4),grade('PSY',4)]).
student(103,'Сиплюсплюсов',[grade('LP',3),grade('MTH',5),grade('FP',3),grade('INF',4),grade('ENG',3),grade('PSY',4)]).
student(103,'Программиро',[grade('LP',3),grade('MTH',5),grade('FP',4),grade('INF',3),grade('ENG',5),grade('PSY',4)]).
student(104,'Джаво',[grade('LP',5),grade('MTH',4),grade('FP',4),grade('INF',5),grade('ENG',3),grade('PSY',4)]).
student(103,'Клавиатурникова',[grade('LP',3),grade('MTH',2),grade('FP',3),grade('INF',2),grade('ENG',5),grade('PSY',4)]).
student(101,'Мышин',[grade('LP',5),grade('MTH',5),grade('FP',2),grade('INF',4),grade('ENG',4),grade('PSY',2)]).
student(104,'Фулл',[grade('LP',5),grade('MTH',4),grade('FP',5),grade('INF',4),grade('ENG',4),grade('PSY',4)]).
student(101,'Безумников',[grade('LP',5),grade('MTH',4),grade('FP',4),grade('INF',4),grade('ENG',5),grade('PSY',4)]).
student(102,'Шарпин',[grade('LP',4),grade('MTH',3),grade('FP',2),grade('INF',3),grade('ENG',3),grade('PSY',4)]).
student(104,'Круглосчиталкин',[grade('LP',5),grade('MTH',4),grade('FP',4),grade('INF',4),grade('ENG',2),grade('PSY',4)]).
student(103,'Решетников',[grade('LP',3),grade('MTH',3),grade('FP',5),grade('INF',5),grade('ENG',5),grade('PSY',4)]).
student(102,'Эксель',[grade('LP',4),grade('MTH',4),grade('FP',4),grade('INF',4),grade('ENG',4),grade('PSY',3)]).
student(102,'Текстописов',[grade('LP',5),grade('MTH',4),grade('FP',5),grade('INF',2),grade('ENG',3),grade('PSY',4)]).
student(103,'Текстописова',[grade('LP',3),grade('MTH',4),grade('FP',3),grade('INF',4),grade('ENG',4),grade('PSY',4)]).
student(101,'Густобуквенникова',[grade('LP',4),grade('MTH',5),grade('FP',4),grade('INF',4),grade('ENG',5),grade('PSY',4)]).
student(102,'Криптовалютников',[grade('LP',4),grade('MTH',3),grade('FP',4),grade('INF',4),grade('ENG',3),grade('PSY',4)]).
student(104,'Блокчейнис',[grade('LP',4),grade('MTH',2),grade('FP',5),grade('INF',2),grade('ENG',5),grade('PSY',3)]).
student(102,'Азурин',[grade('LP',5),grade('MTH',2),grade('FP',5),grade('INF',5),grade('ENG',4),grade('PSY',5)]).
student(103,'Вебсервисов',[grade('LP',4),grade('MTH',5),grade('FP',4),grade('INF',5),grade('ENG',4),grade('PSY',4)]).
student(102,'Круглотличников',[grade('LP',3),grade('MTH',4),grade('FP',5),grade('INF',3),grade('ENG',4),grade('PSY',5)]).


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

% проверка отсутствия несданных экзаменов у даннного студента данной группы
pass(Group, Student, Pass) :- 
    student(Group, Student, Grades), 
    structers_to_grades(Grades, L), 
    (my_member(2, L) -> Pass = false ; Pass = true).

% получение среднего арифмитического оценок и информации о наличии несданных экзаменов данного студента данной группы
student_info(Group, Student, Middle, Pass) :- 
    pass(Group, Student, Pass).
    middle(L, Middle) % 1


count(Elem, [], 0).
count(Elem, [Elem | T], Res) :-
     count(Elem, T, TRes), 
     Res is TRes + 1.
count(Elem, [H | T], Res) :- 
    count(Elem, T, Res).

get_grade(Subj, [grade(Subj, Grade) | _], Grade).
get_grade(Subj, [H | T], Grade) :- 
    get_grade(Subj, T, Grade).

grades_by_subject(Subj, RGrades) :- 
    findall(Grade, (student(_, _, Grades), get_grade(Subj, Grades, Grade)), RGrades).

not_pass_count(SubjName, Count) :- 
    subject(Subj, SubjName), 
    grades_by_subject(SCode, Grades), 
    count(2, Grades, Count). % 2


more_than_everybody_in_list(X, []).
more_than_everybody_in_list(X, [H | T]) :- 
    X >= H, 
    more_than_everybody_in_list(X, T).

max_of_list(L, X) :- 
    my_member(X, L), 
    more_than_everybody_in_list(X, L).

get_middle(Group, Student, Middle) :- 
    student(Group, Student, SGrades), 
    structers_to_grades(SGrades, Grades), 
    middle(Grades, Middle).

get_middles(Group, Middles) :- 
    findall(Middle, get_middle(Group, _, Middle), Middles).

max_middle_student(Group, Student) :- 
    get_middles(Group, Middles), 
    max_of_list(Middles, Max), 
    get_middle(Group, Student, Max).

max_middle(Group, Students) :- 
    findall(Student, max_middle_student(Group, Student), Students). % 3