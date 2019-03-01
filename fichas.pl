:- module(fichas, [ficha/1, lista_fichas/1]).
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

lista_fichas(Ls) :-
  setof(X,ficha(X),Ls0),
  delete(Ls0, 0, Ls).
