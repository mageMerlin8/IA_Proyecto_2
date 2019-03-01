crea_ia_1_basico_prueba:-
  jugador(1),
  asserta(ia_basico(1)),
  write('IA Basico #1 (prueba) creado'),
  jugador_asignar_ficha(1,[5,1]),
  jugador_asignar_ficha(1,[0,0]),
  jugador_asignar_ficha(1,[1,0]),
  jugador_asignar_ficha(1,[3,1]),
  jugador_asignar_ficha(1,[2,2]),
  jugador_asignar_ficha(1,[5,4]),
  jugador_asignar_ficha(1,[5,2]).

crea_ia_2_basico_prueba:-
  jugador(2),
  asserta(ia_basico(2)),
  write('IA Basico #2 (prueba) creado'),
  jugador_asignar_ficha(2,[5,5]),
  jugador_asignar_ficha(2,[2,1]),
  jugador_asignar_ficha(2,[4,1]),
  jugador_asignar_ficha(2,[3,3]),
  jugador_asignar_ficha(2,[3,2]),
  jugador_asignar_ficha(2,[5,0]),
  jugador_asignar_ficha(2,[1,1]).

repite.
repite:-repite.
/*
juega_2_ias:-
  \+ia_basico_juega(1),
  \+ia_basico_juega(2).*/
/*juega_2_ias:-
  repite,
  ia_basico_juega(2),
  ia_basico_juega(1),
  .
*/
juega_ia_prueba:-
  ia_basico_juega(2),
  ia_basico_juega(1).
main:-
  %include('fichas.pl'),
  write('Numero de Jugadores: 2(default)'),nl,
  asignar_jugadores(2),nl,
  crea_ia_1_basico_prueba,
  crea_ia_2_basico_prueba.
:-include(fichas).
