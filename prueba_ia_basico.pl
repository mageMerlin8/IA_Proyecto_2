crea_ia_basico_prueba(NumJs):-
  jugador(NumJs),
  asserta(ia_basico(NumJs)),
  write('IA Basico #'),write(NumJs),write(' (prueba) creado'),nl,
  asigna_piezas_aleatorio(NumJs).

juega_ia_prueba:-
  \+ia_basico_juega(1),
  \+ia_basico_juega(2),!.
juega_ia_prueba:-
  \+ia_basico_juega(2),
  \+ia_basico_juega(1),!.
juega_ia_prueba:-
  juega_ia_prueba.
prueba_1_ia_basico:-
  nuevo_juego,
  write('Prueba entre dos agentes autonomos aleatorios:'),nl,
  asignar_jugadores(2), nl,
  crea_ia_basico_prueba(1),
  crea_ia_basico_prueba(2),
  juega_ia_prueba.
prueba_1_n_veces(0,0,0,0):-!.
prueba_1_n_veces(N,G1,G2,G3):-
  prueba_1_ia_basico,
  M is N-1,
  ganador(Js),
  (
  (Js is  1, prueba_1_n_veces(M,G11,G2,G3), G1 is G11+1,!);
  (Js is  2, prueba_1_n_veces(M,G1,G22,G3), G2 is G22+1,!);
  (Js is -1, prueba_1_n_veces(M,G1,G2,G33), G3 is G33+1,!)
  ).

main:-prueba_1_ia_basico.

:-include(fichas).
