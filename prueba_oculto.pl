:-include(fichas).
main:-
  nuevo_juego,
  write('Prueba entre un agente autonomo aleatorio y humano(oculto):'),nl,
  asignar_jugadores(2), nl,
  crea_ia_basico(1),
  crea_jugador_oculto(2).
:-main.
