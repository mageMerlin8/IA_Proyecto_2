:-include(minimax).

setup:-
  retractall(jugador_ficha_o(_,_)),
  nuevo_juego,
  asignar_jugadores(2),
  lista_fichas_aleatorias(Ls,7),
  crea_ia_minimax(1,Ls),
  crea_jugador_prueba(2),!.
prueba:-
  guitracer,
  spy(jugador_oculto_pasa/1),
  spy(ia_minimax_pasa/1),
  trace,

  repite,
  play_n(1,W,L,T),
  ((W is 1, nl,writeln('ganamos!'),nl);
  (L is 1, nl, writeln('perdimos :('),nl);
  (T is 1, nl,writeln('empate...'),nl)),
  fail.
play_n(0,0,0,0):-!.
play_n(N, W, L, T):-
  setup,
  play,
  M is N-1,
  (
  (winner(1), play_n(M, W1, L, T), W is W1+1,nl,writeln('ganamos!'),!);
  (winner(2), play_n(M, W, L1, T), L is L1+1,nl, writeln('perdimos :('),!);
  (tie,       play_n(M, W, L, T1), T is T1+1,nl,writeln('empate...'),!)
  ).

play:-
  ultimo_turno(T),T>100,!.
play:-
  jugador_turno_valido(1),
  ia_minimax_play(1),fail.
play:-
  (winner(_),!);
  (tie,!).
play:-
  jugador_turno_valido(2),
  jugador_prueba_juega(2),fail.
play:-
  (winner(_),!);
  (tie,!).
play:-play.
winner(1):-
  ganador(1).
winner(2):-
  lista_fichas_jugador_o(2,Ls),Ls=[],ganador(2).
tie:-
  them_moves(1,X),X=[],
  them_moves(2,Y),Y=[],
  the_pot(Pot),Pot=[].
them_moves(1,Ls):-
  lista_fichas_jugador(Mano,1),
  movidas_posibles(Mano,Ls).
them_moves(2,Ls):-
  lista_fichas_jugador_o(2,Mano),
  movidas_posibles(Mano,Ls).
/*
   ___                       _
  |_  |                     | |
    | |_   _  __ _  __ _  __| | ___  _ __
    | | | | |/ _` |/ _` |/ _` |/ _ \| '__|
/\__/ / |_| | (_| | (_| | (_| | (_) | |
\____/ \__,_|\__, |\__,_|\__,_|\___/|_|
              __/ |
             |___/
______                _
| ___ \              | |
| |_/ / __ _   _  ___| |__   __ _
|  __/ '__| | | |/ _ \ '_ \ / _` |
| |  | |  | |_| |  __/ |_) | (_| |
\_|  |_|   \__,_|\___|_.__/ \__,_|
*/
:-dynamic
  jugador_ficha_o/2.
asignar_fichas_o(Js,[Fi|Ls]):-
  asserta(jugador_ficha_o(Js,Fi)),
  write('Ficha_o Asignada: J-'),write(Js),
  write(' F-'),write(Fi),nl,
  asignar_fichas_o(Js,Ls).
asignar_fichas_o(_,[]).
lista_fichas_jugador_o(Js,Ls):-
  findall(X,jugador_ficha_o(Js,X),Ls).
crea_jugador_prueba(Js):-
  crea_jugador_oculto(Js),
  %asignar fichas
  lista_fichas_aleatorias(Ls,7),
  asignar_fichas_o(Js,Ls).
jugador_prueba_juega(Js):-
  %puede mover
  lista_fichas_jugador_o(Js,Mano),
  movidas_posibles(Mano,Mvs),
  length(Mvs,L),L>0,
  random(N),M is floor(N*L),nth0(M,Mvs,Movida),
  nth1(1,Movida,Lado),
  nth1(2,Movida,Fi),
  jugador_oculto_juega(Js,Fi,Lado),!,
  retractall(jugador_ficha_o(Js,Fi)).
jugador_prueba_juega(Js):-
  %tiene que comer
  gimme(Fi),
  jugador_oculto_come(Js),
  asignar_fichas_o(Js,[Fi]),
  jugador_prueba_juega(Js),!.
jugador_prueba_juega(Js):-
  %tiene que pasar (tssssss)
  writeln('Jugador prueba pasa'),
  jugador_oculto_pasa(Js),!.

ia_minimax_play(Js):-
  %puede jugar
  ia_minimax_juega_automatico(Js),!.
ia_minimax_play(Js):-
  %tiene que comer
  gimme(Fi),
  ia_minimax_come(Js,Fi),ia_minimax_play(Js),!.
ia_minimax_play(Js):-
  %tiene que pasar (tssssss)
  writeln('Jugador minimax pasa'),
  ia_minimax_pasa(Js),!.

the_pot(Pot):-
  lista_fichas_disponibles(Disp),
  lista_fichas_jugador_o(2,Lmenos),
  subtract(Disp, Lmenos, Ls),
  \+Ls = [],
  random_permutation(Ls,Pot),!.
the_pot([]).
gimme(Fi):-
  the_pot(P),\+P = [],
  primeros_n_ls(P,1,[Fi]),!.
gimme(_):-!,fail.

:- play_n(1,X,Y,Z),halt.
