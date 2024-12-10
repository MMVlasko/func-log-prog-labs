% Основной предикат для вычисления
calculate(Expression, Result) :-
    parse_expression(Expression, Result, []).

% Разбор выражения: term [+|- rest]
parse_expression(Input, Result, Rest) :-
    parse_term(Input, TermResult, Rest1),
    parse_expression_rest(Rest1, TermResult, Result, Rest).

% Продолжение выражения: [+|- term] или конец
parse_expression_rest(['+' | Rest], Acc, Result, FinalRest) :-
    parse_term(Rest, TermResult, Rest1),
    NewAcc is Acc + TermResult,
    parse_expression_rest(Rest1, NewAcc, Result, FinalRest).
parse_expression_rest(['-' | Rest], Acc, Result, FinalRest) :-
    parse_term(Rest, TermResult, Rest1),
    NewAcc is Acc - TermResult,
    parse_expression_rest(Rest1, NewAcc, Result, FinalRest).
parse_expression_rest(Rest, Acc, Acc, Rest).

% Разбор терма: factor [*|/ rest]
parse_term(Input, Result, Rest) :-
    parse_factor(Input, FactorResult, Rest1),
    parse_term_rest(Rest1, FactorResult, Result, Rest).

% Продолжение терма: [*|/ factor] или конец
parse_term_rest(['*' | Rest], Acc, Result, FinalRest) :-
    parse_factor(Rest, FactorResult, Rest1),
    NewAcc is Acc * FactorResult,
    parse_term_rest(Rest1, NewAcc, Result, FinalRest).
parse_term_rest(['/' | Rest], Acc, Result, FinalRest) :-
    parse_factor(Rest, FactorResult, Rest1),
    NewAcc is Acc / FactorResult,
    parse_term_rest(Rest1, NewAcc, Result, FinalRest).
parse_term_rest(Rest, Acc, Acc, Rest).

% Разбор фактора: base [^ rest]
parse_factor(Input, Result, Rest) :-
    parse_base(Input, BaseResult, Rest1),
    parse_factor_rest(Rest1, BaseResult, Result, Rest).

% Продолжение фактора: [^ base] или конец
parse_factor_rest(['^' | Rest], Acc, Result, FinalRest) :-
    parse_base(Rest, BaseResult, Rest1),
    NewAcc is Acc ** BaseResult,
    parse_factor_rest(Rest1, NewAcc, Result, FinalRest).
parse_factor_rest(Rest, Acc, Acc, Rest).

% Разбор базового значения: число или скобки
parse_base([Number | Rest], Number, Rest) :-
    number(Number).
parse_base(['(' | Rest], Result, FinalRest) :-
    parse_expression(Rest, Result, [')' | FinalRest]).