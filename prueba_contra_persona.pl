:- include(fichas).

prueba_ia_humano:-
  nuevo_juego,
  write('Prueba entre un agente autonomo aleatorio y humano:'),nl,
  asignar_jugadores(2), nl,
  crea_ia_basico(1),
  crea_persona(2).

juega_ia_humano:-
  \+ia_basico_juega(1),
  \+persona_juega(2).
juega_ia_humano:-
  \+persona_juega(2),
  \+ia_basico_juega(1).
juega_ia_humano:-
  juega_ia_humano.
