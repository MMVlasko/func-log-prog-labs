% Основной предикат для вычисления
% Принимает список выражения и переменную, в которую будет записан результат
% Вызывает основной предикат разбора и вычисления, где последний аргумент - остаток
% списка термов в конце вычисления

calculate(Expression, Result) :-
    parse_expression(Expression, Result, []).


% Дальнейшие предикаты работают таким образом, что вначале рекурсивно
% проходят к разбору операций с более высоким приоритетом, а затем возвращаются
% к разбору операций с низким приоритетом
% Здесь сначала вызывается parse_term, который не будкет учитывать + и -,
% а затем parse_expression_rest, который будет искачть и вычислять связанное с + и -

parse_expression(Input, Result, Rest) :-
    parse_term(Input, TermResult, Rest1),
    parse_expression_rest(Rest1, TermResult, Result, Rest).


% Здесь проверяется,что список термов начинается с + или -, и в таком случае 
% вычисляет значение, причём операндами выступают накопленный до этого
% результат (Acc) и результат вычисления для следующей за + или - частью 
% выражения (TermRest); Далее рекурсивно разбирается остаток списка; Если список не начинается с + или -, то в качестве результата возвращается Acc

parse_expression_rest(['+' | Rest], Acc, Result, FinalRest) :-
    parse_term(Rest, TermResult, Rest1),
    NewAcc is Acc + TermResult,
    parse_expression_rest(Rest1, NewAcc, Result, FinalRest).
parse_expression_rest(['-' | Rest], Acc, Result, FinalRest) :-
    parse_term(Rest, TermResult, Rest1),
    NewAcc is Acc - TermResult,
    parse_expression_rest(Rest1, NewAcc, Result, FinalRest).
parse_expression_rest(Rest, Acc, Acc, Rest).


% По аналогии с parse_expression: сначала вызывается разбор
% более приоритетных операций parse_factor, а затем разбор непосредственно * и /

parse_term(Input, Result, Rest) :-    
    parse_factor(Input, FactorResult, Rest1),
    parse_term_rest(Rest1, FactorResult, Result, Rest).

parse_term_rest(['*' | Rest], Acc, Result, FinalRest) :-
    parse_factor(Rest, FactorResult, Rest1),    
    NewAcc is Acc * FactorResult,
    parse_term_rest(Rest1, NewAcc, Result, FinalRest).
parse_term_rest(['/' | Rest], Acc, Result, FinalRest) :-
    parse_factor(Rest, FactorResult, Rest1),    
    NewAcc is Acc / FactorResult,
    parse_term_rest(Rest1, NewAcc, Result, FinalRest).
parse_term_rest(Rest, Acc, Acc, Rest).


% Аналогично для степени ^

parse_factor(Input, Result, Rest) :-
    parse_base(Input, BaseResult, Rest1),
    parse_factor_rest(Rest1, BaseResult, Result, Rest).
parse_factor_rest(['^' | Rest], Acc, Result, FinalRest) :-
    parse_base(Rest, BaseResult, Rest1),
    NewAcc is Acc ** BaseResult,
    parse_factor_rest(Rest1, NewAcc, Result, FinalRest).
parse_factor_rest(Rest, Acc, Acc, Rest).


% Самый глубокий уровень: когда спсиок начинается с числа или скобки
% тогда либо сразу возвращается число

parse_base([Number | Rest], Number, Rest) :-
    number(Number).

% либо для выражения между скобок вызывается вновь разбор верхнего уровня parse_expression

parse_base(['(' | Rest], Result, FinalRest) :-
    parse_expression(Rest, Result, [')' | FinalRest]).