spec('астрономия').
spec('поэзия').
spec('проза').
spec('дрматургия').

name('Алексеев').
name('Константинов').
name('Борисов').
name('Дмитриев').

my_member(H, [H | _]).
my_member(H, [_ | T]) :- 
    my_member(H, T).

single([]).
single([H | T]) :-
   not(my_member(H, T)), single(T).

self_check([]) :- !.
self_check([psngr(_, Read, Buy, Write) | T]) :-
    Read \= Write, Buy \= Write, self_check(T).

get_heads([], []).
get_heads([psngr(H1, _, _, _) | T1], [H2 | T2]) :-
    H1 = H2, get_heads(T1, T2).

solution(X) :-
    X = [psngr(A, ARead, ABuy, AWrite), psngr(B, BRead, BBuy, BWrite), 
        psngr(C, CRead, CBuy, CWrite), psngr(D, DRead, DBuy, DWrite)],
    name(A), name(B), name(C), name(D), single([A, B, C, D]),
    spec(ARead), spec(BRead), spec(CRead), spec(DRead), single([ARead, BRead, CRead, DRead]),
    spec(ABuy), spec(BBuy), spec(CBuy), spec(DBuy), single([ABuy, BBuy, CBuy, DBuy]),
    spec(AWrite), spec(BWrite), spec(CWrite), spec(DWrite), single([AWrite, BWrite, CWrite, DWrite]),
    
    my_member(psngr(_, 'дрматургия', _, 'поэзия'), X),
    not(my_member(psngr('Дмитриев', _, _, 'проза'), X)),
    not(my_member(psngr(_, 'астрономия', _, 'проза'), X)),
    not(my_member(psngr(_, _, 'астрономия', 'проза'), X)),

    not(my_member(psngr(_, _, Buy, Buy), X)),
    not(my_member(psngr(_, Read, _, Read), X)),

    my_member(psngr('Алексеев', BorisBuy, AlexBuy, _), X),
    my_member(psngr('Борисов', AlexBuy, BorisBuy, _), X),
    my_member(psngr('Борисов', _, DmitWrite, _), X),
    my_member(psngr('Дмитриев', _, _, DmitWrite), X),
    get_heads(X, Heads), Heads == ['Алексеев', 'Константинов', 'Борисов', 'Дмитриев'].
