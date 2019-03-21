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

:-thread_local
  turno_p/5,
  jugador_ficha_p/2,
  movida_minmax/2.

:-dynamic
  ia_minimax/1.
  %movida_minmax(mv,num)
asignar_fichas_p(Js,[Fi|Mano]):-
  asserta(jugador_ficha_p(Js,Fi)),
  asignar_fichas_p(Js,Mano).
asignar_fichas_p(_,[]).
mano_jugador_p(Js,Mano):-
  findall(X,jugador_ficha_p(Js,X),Mano),!.
mano_jugador_p(_,[]):-!.
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
  retract(jugador_ficha_p(Jugador,Ficha)),
  ultimo_turno_p(UltT),
  NumTurno is UltT+1,

  asserta(turno_p(Ficha,Jugador,Lado,NumTurno,LL))
  /*write('Turno probable: '),write(NumTurno),nl,
  write('  Jugador:  '),write(Jugador),nl,
  write('  Ficha:    '),write(Ficha),nl,
  write('  Lado:     '),write(Lado),nl,
  write('  LadoLibre:'),write(LL),nl*/.

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
  retraer_turnos_anteriores(Turno,UltT).
retraer_turnos_anteriores(Turno,T1):-
  T1>=Turno,
  turno_p(F1,Js,_,T1,_),
  asserta(jugador_ficha_p(Js,F1)),
  retract(turno_p(_,_,_,T1,_)),
  T2 is T1-1,
  retraer_turnos_anteriores(Turno,T2),!.
retraer_turnos_anteriores(_,_):-!.

jugar_turno_minimax([Lado|[Ficha]],Turno):-
  %quitar todos los turnos anteriores y este
  retraer_turnos_anteriores(Turno),
  %conseguir numero del jugador
  jugador_turno_valido_p(Js),
  %jugar el turno
  jugar_turno_p(Js,Ficha,Lado).
movidas_posibles_minimax(Mvs):-
  jugador_turno_valido_p(Js),
  mano_jugador_p(Js,Mano),
  movidas_posibles(Mano,Mvs).
/*
Funcion de evaluacion basica para dos jugadores ocultos
*/
evalua_1(Val):-
  numero_fichas_jugador_oculto_p(1,J1),
  numero_fichas_jugador_oculto_p(2,J2),
  Val is J2-J1.
evalua_rand(Val):-
  random(X),
  Val is X*10.
  %listing(turno_p).

/*
La siguiente implementacion del algoritmo minimax imita el descrito en:
The Art of Prolog, Second Edition
Advanced Programming Techniques
By Leon S. Sterling and Ehud Y. Shapiro
Capitulo 20
*/
evaluate_and_choose([Mv|Movidas],Turno,D,MaxMin,Record,Best):-
  jugar_turno_minimax(Mv,Turno),
  T2 is Turno+1,
  minimax(D,T2,MaxMin,_,Val),
  update(Mv,Val,Record,R2),
  evaluate_and_choose(Movidas,Turno,D,MaxMin,R2,Best).
evaluate_and_choose([],_,_,_,R,R).
minimax(0,_,MaxMin,_,Val):-
  evalua_rand(Val),
  Val is MaxMin*Val.
minimax(D,Turno,MaxMin,Mv,Val):-
  D>0,
  movidas_posibles_minimax(Movidas),
  D1 is D-1,
  MinMax is -MaxMin,
  evaluate_and_choose(Movidas,Turno,D1,MinMax,[nil,-100],[Mv,Val]).
update(_,Val,[Mv1,V1],[Mv1,V1]):-
   Val < V1.
update(Mv,Val,_,[Mv,Val]).

%con poda alfa beta
evaluate_and_choose_ab([Mv|Movidas],Turno,D,Alfa,Beta,Mv1,Best):-
  jugar_turno_minimax(Mv,Turno),
  T1 is Turno + 1,
  alpha_beta(D,T1,Alfa,Beta,_,Val),
  Val1 is Val,
  cutoff(Mv,Val1,D,Alfa,Beta,Movidas,Turno,Mv1,Best).
%evaluate_and_choose_ab([],_,0,Alfa,_,Mv,[Mv,Alfa]).
evaluate_and_choose_ab([],_,_,Alfa,_,Mv,[Mv,Alfa]).
alpha_beta(0,_,_,_,_,Val):-
  evalua_rand(Val).
alpha_beta(D,Turno,Alfa,Beta,Mv,Val):-
  movidas_posibles_minimax(Movidas),
  Alfa1 is -Beta,
  Beta1 is -Alfa,
  D1 is D - 1,
  evaluate_and_choose_ab(Movidas,Turno,D1,Alfa1,Beta1,nil,[Mv,Val]).
cutoff(Mv,Val,_,_,Beta,_,_,_,[Mv,Val]):-
  Val >= Beta.
cutoff(Mv,Val,D,Alfa,Beta,Movidas,Turno,_,Best):-
  Alfa < Val,
  Val < Beta,
  evaluate_and_choose_ab(Movidas,Turno,D,Val,Beta,Mv,Best).
cutoff(_,Val,D,Alfa,Beta,Movidas,Turno,Mv1,Best):-
  Val =< Alfa,
  evaluate_and_choose_ab(Movidas,Turno,D,Alfa,Beta,Mv1,Best).
/*
 _____  ___   ___  ____       ____  ___
|_   _|/ _ \  |  \/  (_)     (_)  \/  |
  | | / /_\ \ | .  . |_ _ __  _| .  . | __ ___  __
  | | |  _  | | |\/| | | '_ \| | |\/| |/ _` \ \/ /
 _| |_| | | | | |  | | | | | | | |  | | (_| |>  <
 \___/\_| |_/ \_|  |_/_|_| |_|_\_|  |_/\__,_/_/\_\
Este agente solamente va a poder jugar entre 1 de estos y un jugador ocultos (por ahora).
*/
crea_ia_minimax(Js,Mano):-
  jugador(Js),
  asserta(ia_minimax(Js)),
  asignar_fichas_minimax(Js, Mano),!.

asignar_fichas_minimax(Js,[Fi|Mano]):-
  jugador_asignar_ficha(Js,Fi),
  asignar_fichas_minimax(Js,Mano).
asignar_fichas_minimax(_,[]):-!.
fichas_por_jugador([Ji|Js],[Mi|Manos]):-
  jugador_oculto(Ji),
  findall(X,mano_posible_oculto(Ji,X),Mi),
  fichas_por_jugador(Js,Manos),!.
fichas_por_jugador(_,[]):-!.

ia_minimax_acerta_movidas_ini([Movida|Mvs]):-
  assert(movida_minmax(Movida,0)),
  ia_minimax_acerta_movidas_ini(Mvs).
ia_minimax_acerta_movidas_ini([]).

ia_minimax_cambia_valor_movida(Mv,inf):-
  ia_minimax_cambia_valor_movida(Mv,100),!.
ia_minimax_cambia_valor_movida(Mv,-inf):-
  ia_minimax_cambia_valor_movida(Mv,-100),!.
ia_minimax_cambia_valor_movida(Movida,Val):-
  movida_minmax(Movida,X),
  retract(movida_minmax(Movida,X)),
  X1 is X+Val,
  asserta(movida_minmax(Movida,X1)),!.
ia_minimax_cambia_valor_movida(Movida,Val):-
  asserta(movida_minmax(Movida,Val)),!.


ia_minimax_evalua(Js,Resp):-
  lista_fichas_jugador(ManoIA,Js),
  movidas_posibles(ManoIA,Movidas),
  retractall(movida_minmax(_,_)),
  ia_minimax_acerta_movidas_ini(Movidas),
  /*ultimo_turno(UltT),
  T1 is UltT+1,
  D is 4,
  findall(X,jugador_oculto(X),Oponente),
  fichas_por_jugador(Oponente,ManosOp),*/
  findall(X,mano_posible_oculto(2,X),ManosOp),

  length(ManosOp,Tam0),write('Numero de manos posibles: '),writeln(Tam0),

  ia_minimax_evalua(Js,Movidas,ManosOp,ManoIA,Resp).
  %hacer jugador_ficha jugaror_ficha_p
%minimax
/*
ia_minimax_evalua(Js,Movidas,[Mano|Manos],ManoIA,Resp):-
  retractall(jugador_ficha_p(_,_)),
  retractall(turno_p(_,_,_,_,_)),
  asignar_fichas_p(Js,ManoIA),
  jugador_oculto(Jo),
  asignar_fichas_p(Jo,Mano),
  ultimo_turno(UltT),
  T1 is UltT+1,

  nl,writeln(['  Mano siendo analizada: ',Mano]),

  time(evaluate_and_choose(Movidas,T1,4,1,[nil,-100],[Mv,Val])),

  writeln(['    Movida: ',Mv,' Val: ',Val]),

  time(ia_minimax_cambia_valor_movida(Mv,Val)),
  ia_minimax_evalua(Js,Movidas,Manos,ManoIA,Resp).*/
  %acaba minimax
%alfabeta
ia_minimax_evalua(Js,Movidas,[Mano|Manos],ManoIA,Resp):-
  retractall(jugador_ficha_p(_,_)),
  retractall(turno_p(_,_,_,_,_)),
  asignar_fichas_p(Js,ManoIA),
  jugador_oculto(Jo),
  asignar_fichas_p(Jo,Mano),
  ultimo_turno(UltT),
  T1 is UltT+1,

  %nl,writeln(['  Mano siendo analizada: ',Mano]),

  %time()
  evaluate_and_choose_ab(Movidas,T1,4,-inf,inf,_,[Mv,Val]),

  %writeln(['    Movida: ',Mv,' Val: ',Val]),

  ia_minimax_cambia_valor_movida(Mv,Val),
  ia_minimax_evalua(Js,Movidas,Manos,ManoIA,Resp),!.
  %acaba alfabeta


ia_minimax_evalua(_,_,[],_,Resp):-
  findall(X,movida_minmax(_,X),ValMovidas),
  max_list(ValMovidas,MaxVal),
  movida_minmax(Resp,MaxVal),!.

ia_minimax_juega(Js,Fi,La):-
  ia_minimax(Js),
  ficha(Fi),
  lista_fichas_jugador(Fichas,Js),
  member(Fi,Fichas),
  jugar_turno(Js,Fi,La),!.
/*
  lista_fichas_jugador(ManoIA,1),
  movidas_posibles(ManoIA,Movidas),
  ia_minimax_acerta_movidas_ini(Movidas),
  ManoOponente = [[3,2],[4,3],[3,1],[6,5],[2,2]],
  asignar_fichas_p(1,ManoIA),
  asignar_fichas_p(2,ManoOponente),
  ultimo_turno(UltT),
  T1 is UltT+1,
  evaluate_and_choose(Movidas,T1,4,1,[nil,-inf],[Mv,Val]),
  writeln([Mv,Val]),!.
%manos oc
*/

prueba_minimax_wiri(Mano):-
  lista_fichas_jugador(ManoIA,Js),
  movidas_posibles(ManoIA,Movidas),
  ia_minimax_acerta_movidas_ini(Movidas),
  ia_minimax_evalua(Js,Movidas,[Mano],ManoIA,_).
/*
spy points:
ia_minimax_evalua/5
evaluate_and_choose_ab/7
alpha_beta/6
cutoff/9
*/

/*
 _____
/  __ \
| /  \/ ___  _ __   ___ _   _ _ __ _ __ ___ _ __   ___ _   _
| |    / _ \| '_ \ / __| | | | '__| '__/ _ \ '_ \ / __| | | |
| \__/\ (_) | | | | (__| |_| | |  | | |  __/ | | | (__| |_| |
 \____/\___/|_| |_|\___|\__,_|_|  |_|  \___|_| |_|\___|\__, |
                                                        __/ |
                                                       |___/
Vamos a intentar calcular las manos en varios procesos paralelos...
*/

conc_evalua_4(Js,Resp):-
  lista_fichas_jugador(ManoIA,Js),
  movidas_posibles(ManoIA,Movidas),

  %hay que dividir esta lista en cachitos.
  findall(X,mano_posible_oculto(2,X),ManosOp),
  length(ManosOp,Tam0),write('Numero de manos posibles: '),writeln(Tam0),
  random_permutation(ManosOp, ManosOpPer),
  div_4(ManosOpPer,L1,L2,L3,L4),

  concurrent(4,[ia_minimax_evalua(Js,Movidas,L1,ManoIA,R1),
                ia_minimax_evalua(Js,Movidas,L2,ManoIA,R2),
                ia_minimax_evalua(Js,Movidas,L3,ManoIA,R3),
                ia_minimax_evalua(Js,Movidas,L4,ManoIA,R4)],[]),
  Resp = [R1,R2,R3,R4],!.

primeros_n_ls([X1|L1],N,[X1|L2]):-
  N > 0,
  M is N-1,
  primeros_n_ls(L1,M,L2),!.
primeros_n_ls(_,0,[]):-!.

div_4(L,A,B,C,D) :-
  div(L,L1,L2),
  div(L1,A,B),
  div(L2,C,D).

div(L, A, B) :-
  length(L, N),
  Half is N div 2,
  length(A, Half),
  length(B, Half),
  append(A, B, L),!.
div(L, A, B) :-
  length(L, N),
  Half is N div 2 + 1,
  length(A, Half),
  length(B, Half),
  append(A, B, L).
