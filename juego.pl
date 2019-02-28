%ficha_jugador(ficha(X), )
%reparte_fichas(1) :-

main:-
  ['Domino.pl'],
  lista_fichas(X),
  write(X).

lista_fichas([X|Y]) :-
  ficha(X),
  lista_fichas(Y).
