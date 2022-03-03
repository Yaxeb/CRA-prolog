
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



jugar:- generar_tablero_inicial(L), escribir_tablero(L), !.
%%%% código del profe:

lista_repe(1,X,[X]).
lista_repe(N,X,[X|L]):- N1 is N-1, lista_repe(N1,X,L).

generar_tablero_inicial(L) :- lista_repe(7, ' ', L1),
                              lista_repe(6,L1,L).

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

pedir_input(Ficha,TOld):-
    write("Introduzca la columna: "),
    nl, read(Col), 
    introducir_ficha(Col,Ficha,TOld)

introducir_ficha(Col,Ficha,TOld,TNew):- 
    
    %%%validacion.
