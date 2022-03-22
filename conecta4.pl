
jugar:- generar_tablero_inicial(L), escribir_tablero(L), jugando('X', L), !.

jugando('X', L) :- ganador('O', L), write('Gana jugador 2').
jugando('O', L) :- ganador('X', L), write('Gana jugador 1').
jugando(_, L) :- \+ nocompleto(L), write('Empate').
jugando('X', L) :- repeat,write("Elige columna el jugador X"),pedir_input(C, 'X'), jugar_columna('X', C, L, L2),!, escribir_tablero(L2), jugando('O', L2).
jugando('O', L) :- repeat,write("Elige columna el jugador O"),pedir_input(C, 'O'), jugar_columna('O', C, L, L2),!,escribir_tablero(L2), jugando('X', L2).

%%comprueba si el tablero no está completo
nocompleto(L):-  append(_,[C|_],L),
append(_,[' '|_],C),!.

%% VICTORIAS %%

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


%%auxiliar para crear tablero inicial
lista_repe(1,X,[X]).
lista_repe(N,X,[X|L]):- N1 is N-1, lista_repe(N1,X,L).
%%genera el tablero inicial del tipo [[' ', ' ',' ', ' ',' ', ' ', ' '], [' ', ' ',' ', ' ',' ', ' ', ' '], etc..]
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

%%predicados para pedir input a cada uno de los jugadores
pedir_input(C, 'X'):-
    write("Juega 'X', Introduzca la columna: "),
    nl, read(C1), C is C1 - 1, col(C).

pedir_input(C, 'O'):-
    write("Juega 'O', Introduzca la columna: "),
    nl, read(C1), C is C1 - 1, col(C).

%columnas validas
col(0).
col(1).
col(2).
col(3).
col(4).
col(5).
col(6).


%%predicados para trasponer una matriz
transpose([[]|_], []) :- !.
transpose([[I|Is]|Rs], [Col|MT]) :-
    first_column([[I|Is]|Rs], Col, [Is|NRs]),
    transpose([Is|NRs], MT).

first_column([], [], []).
first_column([[]|_], [], []).
first_column([[I|Is]|Rs], [I|Col], [Is|Rest]) :-
    first_column(Rs, Col, Rest).

%%introduce una ficha P en la columna N del tablero L, generando L2
jugar_columna(P, N, L, L3) :- transpose(L, L1), append(I, [C|F], L1),
                                length(I, N), colocar_ficha(P, C, C2), append(I, [C2|F], L2), transpose(L2, L3).

%%coloca una ficha en el fondo de una columna                                
colocar_ficha(P, [' '], [P]) :- !.

%%coloca una ficha encima de otra ficha
colocar_ficha(P, [' ',A|F], [P,A|F]) :- A \== (' '), !.

%%baja por la columna una posición
colocar_ficha(P, [' '|F1], [' '|F2]) :- colocar_ficha(P, F1, F2).
