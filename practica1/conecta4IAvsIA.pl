inicio :- pedir_input(I), nb_setval(contador, 0), jugar(I), !, nb_getval(contador, Valor), write("Partidas ganadas por la IA lista: "), writeln(Valor), nl, 
    write("Partidas ganadas por la IA random: "), R is I - Valor, write(R), nl. 

jugar(1) :- generar_tablero_inicial(L), escribir_tablero(L), jugando('X', L), !.
jugar(I):- generar_tablero_inicial(L), escribir_tablero(L), jugando('X', L), !, I2 is I - 1, jugar(I2).

jugando('X', L) :- ganador('O', L), write('Gana IA lista'), nl, suma.
jugando('O', L) :- ganador('X', L), write('Gana IA random'), nl.
jugando(_, L) :- \+ nocompleto(L), write('Empate'), nl.
jugando('X', L) :- repeat,random(0,7,C), jugar_columna('X', C, L, L2),!,escribir_tablero(L2), jugando('O', L2).
jugando('O', L) :- maquina('O', 'X', L, L2), escribir_tablero(L2), jugando('X', L2).

suma :- nb_getval(contador, C), CNew is C + 1, nb_setval(contador, CNew).

%%comprueba si el tablero no está completo
nocompleto(L):-  append(_,[C|_],L),
append(_,[' '|_],C),!.

%% VICTORIAS %%

%%si puede ganar, gana.
maquina(P,_,L,L2) :- algoritmo(P,L,C,L2), write("maquina: "), write(C), nl, !.

%%si el rival puede ganar, le bloquea
maquina(P,X,L,L2) :- findall((Col,TA), (algoritmo(X,L,_,_), col(Col), jugar_columna(P,Col,L,TA),\+ algoritmo(X,TA,_,_)), [(C,L2)|_]), write("maquina: "), write(C), nl, !.

%%jugar random
maquina(P,_,L,L2) :- repeat,random(0,7,C), jugar_columna(P, C, L, L2), write("maquina: "), write(C), nl.


algoritmo(P,L,C,L2) :- findall((Col,TA), (col(Col), jugar_columna(P,Col,L,TA), ganador(P,TA)), [(C,L2)|_]).

% comprobacion de las columnas
ganador(P, L) :- append(_, [C|_], L),
                 append(_, [P,P,P,P|_], C). % selecciona 4 elementos iguales de la columna 
% comprobacion de las filas
ganador(P, L) :- transpose(L, L1),
                 append(_, [C|_], L1),
                 append(_, [P,P,P,P|_], C).

% comprobando la diagonal de la forma \
ganador(P, L) :- append(_,[C1,C2,C3,C4|_],L),
                 append(I1, [P|_], C1),
                 append(I2, [P|_], C2),
                 append(I3, [P|_], C3),
                 append(I4, [P|_], C4),
                 length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
                 M2 is M1+1, M3 is M2+1, M4 is M3+1. 

% comprobando la diagonal de la forma /
ganador(P, L) :- append(_,[C1,C2,C3,C4|_],L),
                 append(I1, [P|_], C1),
                 append(I2, [P|_], C2),
                 append(I3, [P|_], C3),
                 append(I4, [P|_], C4),
                 length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
                 M2 is M1-1, M3 is M2-1, M4 is M3-1. 

lista_repe(1,X,[X]).
lista_repe(N,X,[X|L]):- N1 is N-1, lista_repe(N1,X,L).

generar_tablero_inicial(L) :- lista_repe(7, ' ', L1),
                              lista_repe(6,L1,L), !.

%%imprime los elementos de una lista
escribir_lista([]).
escribir_lista([X|Y]) :- write(X), escribir_lista(Y).

%%imprime una fila | elemento |...
escribir_lista_con_barra([]).
escribir_lista_con_barra([X|Y]):-
    write(X), write('|'),
    escribir_lista_con_barra(Y).

% imprime el n de columna en la cabecera
escribir_tablero(L):-
    escribir_lista([' ', 1,' ', 2,' ', 3,' ', 4,' ', 5,' ', 6, ' ', 7]), nl,
    escribir_tablero1(L).

escribir_tablero1([]):- lista_repe(15,'-',L1), write(''),
                        escribir_lista(L1), nl.
escribir_tablero1([X|L]):- lista_repe(15,'-',L1), write(''),
                           escribir_lista(L1), nl,
                           write('|'), escribir_lista_con_barra(X), nl,
                           escribir_tablero1(L).

%% generar_tablero_inicial(L), escribir_tablero(L).

%columnas validas
col(0).
col(1).
col(2).
col(3).
col(4).
col(5).
col(6).


transpose([[]|_], []) :- !.
transpose([[I|Is]|Rs], [Col|MT]) :-
    first_column([[I|Is]|Rs], Col, [Is|NRs]),
    transpose([Is|NRs], MT).

first_column([], [], []).
first_column([[]|_], [], []).
first_column([[I|Is]|Rs], [I|Col], [Is|Rest]) :-
    first_column(Rs, Col, Rest).


jugar_columna(P, N, L, L3) :- transpose(L, L1), append(I, [C|F], L1),
                                length(I, N), colocar_ficha(P, C, C2), append(I, [C2|F], L2), transpose(L2, L3).
                                
colocar_ficha(P, [' '], [P]) :- !.
colocar_ficha(P, [' ',A|F], [P,A|F]) :- A \== (' '), !.
colocar_ficha(P, [' '|F1], [' '|F2]) :- colocar_ficha(P, F1, F2).


pedir_input(I):-
    write("Introduzca el numero de partidas que se jugaran: "),
    nl, read(I).
