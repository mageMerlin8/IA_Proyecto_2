:-include(minimax).

main:-
  nuevo_juego,
  write('Prueba del predicado dinamico turno_p y su funcionalidad en minimax.pl'),
  asignar_jugadores(2),nl,
  crea_jugador_oculto(1),
  crea_jugador_oculto(2),
  jugador_oculto_juega(1,[6,6],0),
  jugador_oculto_juega(2,[6,1],1),
  jugador_oculto_juega(1,[6,0],2),
  jugador_oculto_juega(2,[1,1],1),
  jugar_turno_p(1,[2,1],1),
  jugar_turno_p(2,[3,2],1),
  jugar_turno_p(1,[3,3],1).

%movidas_posibles([[5,0],[4,3],[3,0]],X).
