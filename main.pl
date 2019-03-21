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
  read(LsFich),
  crea_ia_minimax(1,LsFich).


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
