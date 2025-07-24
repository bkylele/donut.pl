torus_point(Theta, Phi, A, B, [X,Y,Z]) :-
    X is (2 + cos(Theta)) * (cos(B)*cos(Phi) + sin(A)*sin(B)*sin(Phi)) - sin(Theta)*cos(A)*sin(B),
    Y is (2 + cos(Theta)) * (sin(B)*cos(Phi) - sin(A)*cos(B)*sin(Phi)) + sin(Theta)*cos(A)*cos(B),
    Z is 5 + cos(A)*(2 + cos(Theta))*sin(Phi) + sin(Theta)*sin(A).


point_projection([X,Y,Z], [XP,YP]) :-
    XP is floor(15 + 18 * (1/Z) * X),
    YP is floor(15 - 18 * (1/Z) * Y).


luminance(Theta, Phi, A, B, R) :-
    R is cos(Phi)*cos(Theta)*sin(B)
        - cos(A)*cos(Theta)*sin(Phi)
        - sin(A)*sin(Theta)
        + cos(B)*(cos(A)*sin(Theta) - cos(Theta)*sin(A)*sin(Phi)).


luminance_ascii(L, Char) :-
    Chars = '.,-~:;=!*#$@',
    Index is floor(L * 8),
    sub_atom(Chars, Index, 1, _, Char).


between(Low, High, _, Value) :-
    Low =< High,
    Value = Low.
between(Low, High, Step, Value) :-
    Low < High,
    L1 is Low + Step,
    between(L1, High, Step, Value).


points(A, B, Ps) :-
    aggregate_all(set([XP,YP,L]),
    (
        between(0, 2*pi, 0.10, T),
        between(0, 2*pi, 0.04, P),
        torus_point(T,P,A,B,[X,Y,Z]),
        luminance(T, P, A, B, L),
        point_projection([X,Y,Z], [XP,YP]),
        L > 0
    ), Points),
    once(front_points(Points, Ps)).


front_points([], []).
front_points([[X,Y,Z]], [[X,Y,Z]]).
front_points([[X,Y,_], [X,Y,Z1]|P0s], Ps) :-
    front_points([[X,Y,Z1]|P0s], Ps).
front_points([[X0,Y0,Z0],[X1,Y1,Z1]|P0s], [[X0,Y0,Z0]|Ps]) :-
    (dif(X0,X1) ; dif(Y0,Y1)),
    front_points([[X1,Y1,Z1]|P0s], Ps).


show(Ps,X,Y) :-
    member([X,Y,L], Ps), !
->  luminance_ascii(L, Char),
    write(Char)
;   write(" ").


render(A,B) :-
    points(A,B,Ps),
    write("\e[H"),
    foreach(between(0,30,Y),
            (foreach(between(0,30,X), show(Ps, X, Y)), nl)).


main :-
    foreach((between(0, 3*2*pi, 0.1, I), render(I,I)), true).

