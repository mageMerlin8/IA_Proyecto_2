:-use_module(juego).

main2 :-
  write('Numero de Jugadores:'),nl,
  read(NumJs),
  asignar_jugadores(NumJs).
