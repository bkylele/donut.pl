:- use_module(library(ansi_term)).
:- use_module(library(matrix)).
:- use_module(library(lambda)).

:- arithmetic_function(screen_width/0).
:- arithmetic_function(screen_height/0).
:- arithmetic_function(r1/0).
:- arithmetic_function(r2/0).
:- arithmetic_function(k1/0).
:- arithmetic_function(k2/0).
:- arithmetic_function(theta_spacing/0).
:- arithmetic_function(phi_spacing/0).

screen_width(30).
screen_height(30).
r1(1).
r2(2).
k1(X) :- X is (screen_width*k2*3) / (8*(r1+r2)). % represents distance from viewer to screen (?)
k2(5).
theta_spacing(0.07).
phi_spacing(0.02).
light_dir(0, 1, -1).


% rotate_x(T, V, R) :-
%     RotM = [[1,     0,       0],
%             [0,     cos(T),  sin(T)],
%             [0,    -sin(T),  cos(T)]],
%     matrix_multiply(V, RotM, R).

% rotate_y(T, V, R) :-
%     RotM = [[cos(T),  0,  -sin(T)],
%             [0,       1,  0],
%             [sin(T),  0,  cos(T)]],
%     matrix_multiply(V, RotM, R).

% rotate_z(T, V, R) :-
%     RotM = [[cos(T),    sin(T),  0],
%             [-sin(T),   cos(T),  0],
%             [0,         0,       1]],
%     matrix_multiply(V, RotM, R).


% Relates the angles from the center of the torus and the center of the tube to
% a point on its surface.
torus_point(Theta, Phi, A, B, [X,Y,Z]) :-
    SinTheta is sin(Theta)  , CosTheta is cos(Theta),
    SinPhi is sin(Phi)      , CosPhi is cos(Phi),
    SinA is sin(A)          , CosA is cos(A),
    SinB is sin(B)          , CosB is cos(B),
    X is (r2 + r1*CosTheta) * (CosB*CosPhi + SinA*SinB*SinPhi) - r1*SinTheta*CosA*SinB,
    Y is (r2 + r1*CosTheta) * (SinB*CosPhi - SinA*CosB*SinPhi) + r1*SinTheta*CosA*CosB,
    Z is k2 + CosA*(r2 + r1 * CosTheta)*SinPhi + r1*SinTheta*SinA.


point_projection([X,Y,Z], [XP,YP]) :-
    XP is floor((screen_width/2) + k1 * (1/Z) * X),
    YP is floor((screen_height/2) - k1 * (1/Z) * Y).


luminance(Theta, Phi, A, B, R) :-
    SinTheta is sin(Theta)  , CosTheta is cos(Theta),
    SinPhi is sin(Phi)      , CosPhi is cos(Phi),
    SinA is sin(A)          , CosA is cos(A),
    SinB is sin(B)          , CosB is cos(B),
    R is CosPhi*CosTheta*SinB
        - CosA*CosTheta*SinPhi
        - SinA*SinTheta
        + CosB*(CosA*SinTheta - CosTheta*SinA*SinPhi).


luminance_ascii(L, Char) :-
    Li is floor(L * 8),
    lascii_(Li, Char).
lascii_(0, ".").
lascii_(1, ",").
lascii_(2, "-").
lascii_(3, "~").
lascii_(4, ":").
lascii_(5, ";").
lascii_(6, "=").
lascii_(7, "!").
lascii_(8, "*").
lascii_(9, "#").
lascii_(10,"$").
lascii_(11,"@").


between(Low, High, _, Value) :-
    Low =< High,
    Value = Low.
between(Low, High, Step, Value) :-
    Low < High,
    L1 is Low + Step,
    between(L1, High, Step, Value).


front_points([], []).
front_points([[X,Y,Z]], [[X,Y,Z]]).
front_points([[X,Y,_], [X,Y,Z1]|P0s], Ps) :-
    front_points([[X,Y,Z1]|P0s], Ps).
front_points([[X0,Y0,Z0],[X1,Y1,Z1]|P0s], [[X0,Y0,Z0]|Ps]) :-
    (dif(X0,X1) ; dif(Y0,Y1)),
    front_points([[X1,Y1,Z1]|P0s], Ps).


create_buffer(A,B) :-
    screen_height(H), screen_width(W),

    aggregate_all(set([XP,YP,L]),
    (
        between(0, 2*pi, 0.07, T),
        between(0, 2*pi, 0.02, P),
        torus_point(T,P,A,B,[X,Y,Z]),
        luminance(T, P, 0, 0, L),
        point_projection([X,Y,Z], [XP,YP]),
        L > 0
    ), Points),
    once(front_points(Points, FPoints)),

%     ansi_format([], "\e[H", []),
%     findall(Y, between(0,H,Y), Ys),
%     maplist(\Y^
%     (
%         findall(X, between(0,W,X), Xs),
%         maplist(\X^
%         (
%             member([X,Y,L], FPoints)
%         ->  luminance_ascii(L, Char),
%             write(Char)
%         ;   write(" ")
%         ), Xs),
%         nl
%     ), Ys)
    write("done"), nl
    .


run(A,B) :-
    A < 2*pi,
    create_buffer(A,B),
    run(A+0.01, B+0.01).

main :-
    write('\e[?25l'),
    run(0,0).
