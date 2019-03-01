%ficha_jugador(ficha(X), )
%reparte_fichas(1) :-
:- module(juego, [jugador_ficha/2,
                  jugador_ficha_valida/2,
                  asignar_jugadores/1,
                  turno/4,
                  jugador/1,
                  lado/1,
                  jugador_asignar_ficha/2]).
:- use_module(fichas).
:- use_module(library(lists), [member/2 as member]).
:- dynamic
      jugador_ficha/2,
      turno/4,
      jugador/1.

asignar_jugadores(0) :-!.
asignar_jugadores(N) :-
  N =< 4,
  asserta(jugador(N)),
  M is N-1,
  asignar_jugadores(M).


/*
jugador_ficha(J,F) :- indica que la ficha F le pertenece a jugador J.
*/
jugador_ficha(0,0).
jugador_ficha_valida(Js,Fi):-
  jugador(Js),
  fichas:ficha(Fs),!.
/*
jugador_asignar_ficha(J,F) :- Le asigna la ficha F al jugador J. Checa que sea
                              valida la asignacion.
*/
jugador_asignar_ficha(Js,Fi):-
  jugador_ficha_valida(Js,Fi),
  setof(X, jugador_ficha(Z,X), ListaFichasAsignadas),
  (member(Fi, ListaFichasJugadas) is false),
  asserta(jugador_ficha(Js,Fi)).
%jugador_asignar_ficha(0,0).
/*
turno(Ficha, jugador, lado, numTurno)
*/
turno(0,0,0,0).
/*ultima_ficha_lado(SalidaFicha, Lado) :-
  setof(F0, turno(F0,_,Lado,_))
jugar_turno(Jugador, Ficha, Lado) :-
  %Validar turno

  %folio

  %acertar
  .
*/
lado(0).
lado(1).
lado(2).
