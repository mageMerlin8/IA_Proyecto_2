:-include(minimax).

main:-
    nuevo_juego,
    write('Prueba del predicado dinamico turno_p y su funcionalidad en minimax.pl'),
    asignar_jugadores(2),nl,
    crea_ia_minimax(1,[[6,6],[6,0],[2,1],[3,3],[2,0],[5,3],[5,1]]),
    crea_jugador_oculto(2),
    ia_minimax_juega(1,[6,6],0),
    jugador_oculto_juega(2,[6,1],1),
    ia_minimax_juega(1,[6,0],2),
    jugador_oculto_juega(2,[1,1],1).
main_debug:-
  main,
  guitracer,
  spy(ia_minimax_evalua/5),
  spy(evaluate_and_choose_ab/7),
  spy(alpha_beta/6),
  spy(cutoff/9),
  trace,
  ia_minimax_evalua(1,X),writeln(X).
/*
algunas manos posibles de jugador_oculto(2) despues de main:
X = [[0, 0], [1, 0], [2, 2], [3, 0], [3, 1]] ;
X = [[0, 0], [1, 0], [2, 2], [3, 0], [3, 2]] ;
X = [[0, 0], [1, 0], [2, 2], [3, 0], [4, 0]] ;
X = [[0, 0], [1, 0], [2, 2], [3, 0], [4, 1]] ;
X = [[0, 0], [1, 0], [2, 2], [3, 0], [4, 2]] ;
X = [[0, 0], [1, 0], [2, 2], [3, 0], [4, 3]] ;
X = [[0, 0], [1, 0], [2, 2], [3, 0], [4, 4]] ;
X = [[0, 0], [1, 0], [2, 2], [3, 0], [5, 0]] ;
*/

main2:-
    nuevo_juego,
    write('Prueba del predicado dinamico turno_p y su funcionalidad en minimax.pl'),
    asignar_jugadores(2),nl,
    crea_ia_minimax(1,[[6,6],[6,0],[2,1],[3,3],[2,0],[5,3],[5,1]]),
    crea_jugador_oculto(2),
    ia_minimax_juega(1,[6,6],0),
    jugador_oculto_juega(2,[6,1],1),
    ia_minimax_juega(1,[6,0],2),
    jugador_oculto_juega(2,[1,1],1),
    ia_minimax_juega(1,[5,1],1),
    jugador_oculto_juega(2,[4,0],2).
:-main_debug.
