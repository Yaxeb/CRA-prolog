
%jugar:- iniciarTablero(X), mostrar(X) !.

%iniciarTablero(tablero([' ',' ',' ',' ',' ',' ',' '],
    %                   [' ',' ',' ',' ',' ',' ',' '],
   %                    [' ',' ',' ',' ',' ',' ',' '],
  %                     [' ',' ',' ',' ',' ',' ',' '],
 %                      [' ',' ',' ',' ',' ',' ',' '],
%                       [' ',' ',' ',' ',' ',' ',' '])).

%mostrar(tablero):- print(' A B C D E F G' ), nl,
%           escribirFila(tablero, 6).

%escribirFila('_',0).
%escribirFila(X,N):- escribirFila(X2, N),
%                    Nn is N-1,


%mostrar(tablero).



jugar:- generar_tablero_inicial(L), escribir_tablero(L), jugando('X', L), !.

%%jugando('X', L) :- ganador('O', L), write('Gana jugador 2').
%%jugando('O', L) :- ganador('X', L), write('Gana jugador 1').
%%jugando(_, L) :- completo(L), write('Empate').
jugando('X', L) :- pedir_input(C), write(C), nl, escribir_tablero(L), jugando('O', L).
jugando('O', L) :- pedir_input(C), write(C), nl, escribir_tablero(L), jugando('X', L).

%% VICTORIAS %%

%%%% código del profe:

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

pedir_input(C):-
    write("Introduzca la columna: "),
    nl, read(C1), C is C1 - 1, col(C).

%columnas validas
col(0).
col(1).
col(2).
col(3).
col(4).
col(5).
col(6).


%%colocar(P, C, L, L1) :-
%%introducir_ficha(Col,Ficha,TOld,TNew):-
    
    %%%validacion.

