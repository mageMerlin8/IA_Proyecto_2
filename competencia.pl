:-include(minimax).
setup_competencia:-
  retractall(jugador_ficha_o(_,_)),
  nuevo_juego,
  asignar_jugadores(2),
  crea_ia_minimax(1),
  crea_jugador_oculto(2),
  writeln('Ingresa la lista de fichas de ia:'),
  read(X),
  asignar_fichas_minimax(1,X).
