spec('астрономия').
spec('поэзия').
spec('проза').
spec('дрматургия').

% сопоставления для корректного вывода информации в решении
equal('астрономия', 'астроном', 'книгу по астрономии').
equal('поэзия', 'поэт', 'поэму').
equal('проза', 'прозаик', 'прозу').
equal('дрматургия', 'драматург', 'пьесу').

my_member(H, [H | _]).
my_member(H, [_ | T]) :- 
    my_member(H, T).

my_length([], 0).
my_length([_ | T], Length) :- 
    my_length(T, TLength), 
    Length is TLength + 1.

% проверка, является ли каждый элемент списка уникальным в нём
single([]).
single([H | T]) :-
   not(my_member(H, T)), single(T).

solution(X) :-
    X = [['Алексеев', ARead, ABuy, AWrite], ['Константинов', BRead, BBuy, BWrite], 
        ['Борисов', CRead, CBuy, CWrite], ['Дмитриев', DRead, DBuy, DWrite]],

    % получение читаемой, купленной и написанной книги, проверка, 
    % что соответствующие жанры у пассажиров не повторяются
    spec(ARead), spec(BRead), spec(CRead), spec(DRead), 
    single([ARead, BRead, CRead, DRead]),
    spec(ABuy), spec(BBuy), spec(CBuy), spec(DBuy), 
    single([ABuy, BBuy, CBuy, DBuy]),
    spec(AWrite), spec(BWrite), spec(CWrite), spec(DWrite), 
    single([AWrite, BWrite, CWrite, DWrite]),
    
    % поэт не читает пьесу
    my_member([_, 'дрматургия', _, 'поэзия'], X),
    
    % Дмитриев не прозаик 
    % not(my_member(['Дмитриев', _, _, 'проза'], X)),
    
    % Прозаик не читает астрономию
    not(my_member([_, 'астрономия', _, 'проза'], X)),
    
    % и не покупал ?
    % not(my_member([_, _, 'астрономия', 'проза'], X)),

    % никто не покупал и не читает свою книгу
    not(my_member([_, _, Buy, Buy], X)),
    not(my_member([_, Read, _, Read], X)),

    % Алексеев и Борисов обменялись книгами
    my_member(['Алексеев', BorisBuy, AlexBuy, _], X),
    my_member(['Борисов', AlexBuy, BorisBuy, _], X),

    % Борисов купил книгу Дмитриева
    my_member(['Борисов', _, DmitWrite, _], X),
    my_member(['Дмитриев', _, _, DmitWrite], X).

% печать данных об одном пассажире
write_man([Name, Read, Write]) :-
    write(Name),
    write(' - '),
    equal(Write, Spec, _),
    write(Spec),
    write(', он читает '),
    equal(Read, _, Book),
    write(Book), nl.

% печать списка данных о пассажирах
write_list([]).
write_list([[A, B, C, D] | T]) :-
    write_man(A), 
    write_man(B), 
    write_man(C), 
    write_man(D), nl, 
    write_list(T).

% получение списка данных о пассажирах без учёта купленной книги
miss_buy([], []).
miss_buy([[Name, Read, _, Write] | T1], [[Name, Read, Write] | T2]) :-
    miss_buy(T1, T2).

% получение списка уникальных элементов данного списка
unique_elements([], []).
unique_elements([H | T], Result) :-
    my_member(H, T),
    unique_elements(T, Result), !.
unique_elements([H | T], [H | Result]) :-
    not(my_member(H, T)),
    unique_elements(T, Result).

% вывод всех возможных решений
solve :-
    findall(Res, (solution(X), miss_buy(X, Res)), All),  
    unique_elements(All, Final), 
    write_list(Final).