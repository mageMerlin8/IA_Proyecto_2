/*
ficha([X|Y]) :- se definen los numeros a usar en las fichas.
                Despues se da una definicion de las fichas donde
                cada elemento es uno de los numeros y el primer numero
                es mayor o igual. Esto es para evitar que se repitan fichas:
                ficha([6|5]) es una ficha valida y ficha([5|6]) no lo es.
                Ademas se define ficha(0) para representar cuando algun jugador
                haya pasado.
*/
numero(6).
numero(5).
numero(4).
numero(3).
numero(2).
numero(1).
numero(0).


ficha([X,Y]):-
  numero(X),
  numero(Y),
  Y =< X.
ficha(0).
/*
turno(i, i) :-
*/
lado(0).
lado(1).
lado(2).
jugador(1).
jugador(2).
jugador(3).
jugador(4).

turno(ficha(X), jugador(Y), lado(Z)).
