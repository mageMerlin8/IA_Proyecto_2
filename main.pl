:-include(minimax).
/*
main:-
  writeln(' DumDom  '),
  writeln('---------'),
*/

setup_2_js:-
  nuevo_juego,
  asignar_jugadores(2),
  crea_jugador_oculto(2),
  writeln('Se creo un juego nuevo para dos jugadores.'),
  writeln('Jugador #1 es inteligente (minimax),'),
  writeln('Jugador #2 es oculto.'),nl,
  write('Por favor ingresa la lista de fichas del jugador.'),
  writeln('(Recuerda que las fichas son una lista de dos numeros menores a 6 y el primero es mayor o igual al segundo):'),
  lee_fichas(LsFich,7),
  crea_ia_minimax(1,LsFich).

setup_p_2_js:-
  nuevo_juego,
  asignar_jugadores(2),
  crea_jugador_oculto(2),
  lista_fichas_aleatorias(LsFich,7),
  crea_ia_minimax(1,LsFich),
  lista_fichas_aleatorias(LJug,7),
  write('lista fichas del jugador: '),writeln(LJug).


prueba:-
  nuevo_juego,
  asignar_jugadores(2),
  crea_jugador_oculto(2),
  crea_ia_minimax(1,[[4,4],[2,0],[5,0],[4,1],[4,3],[3,3],[3,2]]),
  ia_minimax_juega(1,[4,4],0),
  jugador_oculto_juega(2,[4,2],2),
  ia_minimax_juega(1,[2,0],2),
  jugador_oculto_juega(2,[0,0],2),
  ia_minimax_juega(1,[5,0],2),
  jugador_oculto_juega(2,[5,1],2),
  ia_minimax_juega_automatico(1),
  %guitracer,
  %spy(ultima_ficha_lado/2),
  %trace,
  jugador_oculto_come(2).

/*
Instrucciones:
1.

*/
lee_fichas([],0).
lee_fichas([Fi|Lf],N):-
  write('Dame primer numero:  '),read(F1),
  write('Dame segundo numero: '),read(F2),
  Fi = [F1,F2],
  ficha(Fi),
  M is N-1,
  lee_fichas(Lf,M).
