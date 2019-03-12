/*
___  ____       _
|  \/  (_)     (_)
| .  . |_ _ __  _ _ __ ___   __ ___  __
| |\/| | | '_ \| | '_ ` _ \ / _` \ \/ /
| |  | | | | | | | | | | | | (_| |>  <
\_|  |_/_|_| |_|_|_| |_| |_|\__,_/_/\_\

aqui va todo el desmadre de minimax.
*/
:-include(fichas).
/*
Se definen algunos predicados dinamicos para la funcionalidad del algoritmo
minimax. El manejo de estos va a ser un poco delicado ya que vamos a afirmar y
retraer constantemente durante la evaluacion minimax.

turno_p(ficha, jugador, lado, num_turno, lado_libre):-
    Hecho dinamico que representa un turno probable, es decir un estado del
    arbol minimax.
*/


:-dynamic
  turno_p/5,
  jugador_ficha_p/2.

asignar_fichas_p(Js,[Fi|Mano]):-
  asserta(jugador_ficha_p(Js,Fi)),
  asignar_fichas_p(Js,Mano).
asignar_fichas_p(_,[]).
/*
Primero se replica toda la funcionalidad del juego dentro del agente inteligente
para que pueda construir los arboles de juego usando la misma logica de juego y
los predicados dinamicos _p 'p de posibles'.
*/
ultimo_turno_p(T):-
  findall(X, turno_p(_,_,_,X,_), TurnosP),
  max_list(TurnosP, T),!.
ultimo_turno_p(T):-
  ultimo_turno(T),!.

ultimo_jugador_p(J):-
  ultimo_turno_p(T),
  (turno_p(_,J,_,T,_);
  turno(_,J,_,T,_)),!.

primer_turno_p(T):-
  findall(X,turno_p(_,_,_,X,_),Ls),
  min_list(Ls, T),!.
primer_turno_p(inf).

ultima_ficha_lado_p(Lado,Ficha):-
  ultimo_turno_p(T),
  primer_turno_p(T1),
  T < T1,
  ultima_ficha_lado(Lado,Ficha),!.

ultima_ficha_lado_p(Lado, Ficha) :-
  ultimo_turno_p(T),
  (
  (turno_p(Ficha,_,Lado,T,_),!);
  (T1 is T-1,ultima_ficha_lado_p(Lado, Ficha, T1))
  ).
%Funciones auxiliares
ultima_ficha_lado_p(Lado,Ficha,T):-
  primer_turno_p(PT),
  T < PT,
  ultima_ficha_lado(Lado, Ficha, T),!.
%TODO: Considerar quitar el caso donde turno es -1 ya que el primer turno_p no sera menor a 0
ultima_ficha_lado_p(_, Ficha, -1):-
  turno_p(Ficha,_,_,1,_),!.
ultima_ficha_lado_p(Lado, Ficha, T) :-
  (turno_p(Ficha,_,Lado,T,_),!);
  (T1 is T-1,ultima_ficha_lado_p(Lado, Ficha, T1)).

ficha_num_libre_p([X,Y],X):-
  (
  turno([X,Y],_,_,_,1);
  turno_p([X,Y],_,_,_,1)
  ),!.
ficha_num_libre_p([X,Y],Y):-
  (
  turno([X,Y],_,_,_,2);
  turno_p([X,Y],_,_,_,2)
  ),!.
ficha_num_libre_p([X,Y],X):-
  (
  turno([X,Y],_,0,1,0);
  turno_p([X,Y],_,0,1,0)
  ),\+turno(_,_,1,_,_),!.
ficha_num_libre_p([X,Y],Y):-
  (
  turno([X,Y],_,0,1,0);
  turno_p([X,Y],_,0,1,0)
  ),\+turno(_,_,2,_,_),!.

ficha_turno_valida_p(Ficha, Lado, 1):-
  ultima_ficha_lado_p(Lado,UFicha),
  ficha_num_libre_p(UFicha,ULibre),
  nth1(2, Ficha, ULibre),!.

ficha_turno_valida_p(Ficha, Lado, 2):-
  ultima_ficha_lado_p(Lado,UFicha),
  ficha_num_libre_p(UFicha,ULibre),
  nth1(1, Ficha, ULibre),!.

jugador_turno_valido_p(Jugador):-
  ultimo_turno_p(UltT),
  primer_turno_p(PPT),
  UltT < PPT,
  jugador_turno_valido(Jugador).

jugador_turno_valido_p(Jugador):-
  max_jugadores(MaxJs),
  ultimo_jugador_p(UltJs),
  (
  (Jugador is UltJs + 1, jugador(Jugador),!);
  (Jugador is (UltJs+1) rem MaxJs, jugador(Jugador),!)
  ).




jugar_turno_p(Jugador, Ficha, Lado):-
  jugador(Jugador),
  ficha(Ficha),
  lado(Lado),

  jugador_turno_valido_p(Jugador),
  ficha_turno_valida_p(Ficha, Lado, LL),

  ultimo_turno_p(UltT),
  NumTurno is UltT+1,

  asserta(turno_p(Ficha,Jugador,Lado,NumTurno,LL)),
  write('Turno probable: '),write(NumTurno),nl,
  write('  Jugador:  '),write(Jugador),nl,
  write('  Ficha:    '),write(Ficha),nl,
  write('  Lado:     '),write(Lado),nl,
  write('  LadoLibre:'),write(LL),nl.

%MV de la forma: [Lado,Ficha([X1,X2])]
movidas_posibles([],[]).
movidas_posibles([Fi|Mano],[[1,Fi],[2,Fi]|Movidas]):-
  ficha_turno_valida_p(Fi,1,_),
  ficha_turno_valida_p(Fi,2,_),
  movidas_posibles(Mano,Movidas),!.
movidas_posibles([Fi|Mano],[[1,Fi]|Movidas]):-
  ficha_turno_valida_p(Fi,1,_),
  movidas_posibles(Mano,Movidas),!.
movidas_posibles([Fi|Mano],[[2,Fi]|Movidas]):-
  ficha_turno_valida_p(Fi,2,_),
  movidas_posibles(Mano,Movidas),!.
movidas_posibles([_|Mano],Movidas):-
  movidas_posibles(Mano,Movidas).

numero_fichas_jugador_oculto_p(Js,Num):-
  numero_fichas_jugador_oculto(Js,Num1),
  (
  (findall(X,turno_p(X,Js,_,_,_),JugadasP),!);
  JugadasP = []
  ),
  length(JugadasP,NumJP),
  Num is Num1-NumJP.

retraer_turnos_anteriores(Turno):-
  ultimo_turno_p(UltT),
  \+retraer_turnos_anteriores(Turno,UltT).
retraer_turnos_anteriores(Turno,T1):-
  T1>=Turno,
  retract(turno_p(_,_,_,T1,_)),
  T2 is T1-1,
  retraer_turnos_anteriores(Turno,T2).


jugar_turno_minimax([Lado|Ficha],Turno):-
  %quitar todos los turnos anteriores y este
  retraer_turnos_anteriores(Turno),
  %conseguir numero del jugador
  jugador_turno_valido_p(Js),
  %jugar el turno
  jugar_turno_p(Js,Ficha,Lado).

/*
Funcion de evaluacion basica para dos jugadores ocultos
*/
evalua_1(Val):-
  numero_fichas_jugador_oculto_p(1,J1),
  numero_fichas_jugador_oculto_p(2,J2),
  Val is J2-J1.
