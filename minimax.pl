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
nuevo_juego :-
  retractall(jugador_ficha(_,_)),
  retractall(turno(_,_,_,_,_)),
  retractall(jugador_oculto_no_tiene(_,_)),
  retractall(oculto_comio(_,_)),
  retractall(jugador(_)),
  retractall(max_jugadores(_)),
  retractall(ia_basico(_)),
  retractall(jugador_persona(_)),
  retractall(jugador_oculto(_)),
  retractall(ia_minimax(_)),
  assert(turno(0,0,0,0,0)).

lista_fichas_aleatorias(Fis,Tam0):-
  lista_fichas_disponibles(Disp),
  random_permutation(Disp,Rand),
  primeros_n_ls(Rand,Tam0,Fis).

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
  findall(X,turno_p(_,_,Lado,X,_),Ls),
  max_list(Ls,T),
  turno_p(Ficha,_,_,T,_),!.
ultima_ficha_lado_p(Lado,Ficha):-
  ultima_ficha_lado(Lado,Ficha).
%
% ultima_ficha_lado_p(Lado,Ficha):-
%   ultimo_turno_p(T),
%   primer_turno_p(T1),
%   T < T1,
%   ultima_ficha_lado(Lado,Ficha),!.
%
% ultima_ficha_lado_p(Lado, Ficha) :-
%   ultimo_turno_p(T),
%   (
%   (turno_p(Ficha,_,Lado,T,_),!);
%   (T1 is T-1,ultima_ficha_lado_p(Lado, Ficha, T1))
%   ).
% %Funciones auxiliares
% ultima_ficha_lado_p(Lado,Ficha,T):-
%   primer_turno_p(PT),
%   T < PT,
%   ultima_ficha_lado(Lado, Ficha, T),!.
%TODO: Considerar quitar el caso donde turno es -1 ya que el primer turno_p no sera menor a 0
ultima_ficha_lado_p(_, Ficha, -1):-
  turno_p(Ficha,_,_,1,_),!.
ultima_ficha_lado_p(Lado, Ficha, T) :-
  (turno_p(Ficha,_,Lado,T,_),!);
  (T1 is T-1,ultima_ficha_lado_p(Lado, Ficha, T1)).

ficha_num_libre_p(F,Li):-
  turno(F,_,_,_,_),
  ficha_num_libre(F,Li).
ficha_num_libre_p([X,Y],X):-
  turno_p([X,Y],_,_,_,1),!.
ficha_num_libre_p([X,Y],Y):-
  turno_p([X,Y],_,_,_,2),!.

ficha_num_libre_p([X,Y],Y):-
  (
  turno([X,Y],_,0,1,0);
  turno_p([X,Y],_,0,1,0)
  ),
  \+turno(_,_,2,_,_),
  \+(turno([Y,_],_,1,_,2);
     turno([_,Y],_,1,_,2)).


ficha_turno_valida_p(Ficha, Lado, LL):-
  ultima_ficha_lado(Lado, UltimaFicha),
  turno(UltimaFicha,_,_,_,UltLibre),
  UltLibre is 0,!,
  ficha_turno_valida(Ficha,Lado,LL),!.
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

  asserta(turno_p(Ficha,Jugador,Lado,NumTurno,LL))/*
  write('Turno probable: '),write(NumTurno),nl,
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

numero_fichas_jugador_p(Js,Num):-
  (
  (findall(X,jugador_ficha_p(Js,X),Fis),!);
  Fis = []
  ),
  length(Fis,Num).

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
evalua_1_2Js(Val):-
  ultimo_jugador(J1),
  jugador_turno_valido_p(J2),
  numero_fichas_jugador_p(J1,F1),
  numero_fichas_jugador_p(J2,F2),
  Val is F1-F2.

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
  Val1 is -Val,
  cutoff(Mv,Val1,D,Alfa,Beta,Movidas,Turno,Mv1,Best).
%llego al final del arbol
evaluate_and_choose_ab([],_,_,Alfa,_,Mv,[Mv,Alfa]).
alpha_beta(0,_,_,_,_,Val):-
  evalua_1_2Js(Val).
%gano
alpha_beta(D,_,_,_,_,Val):-
  ultimo_jugador_p(Js),
  numero_fichas_jugador_p(Js,0),
  Val is D + 10.
%normal
alpha_beta(D,Turno,Alfa,Beta,Mv,Val):-
  movidas_posibles_minimax(Movidas),
  \+(Movidas = []),
  Alfa1 is -Beta,
  Beta1 is -Alfa,
  D1 is D - 1,
  evaluate_and_choose_ab(Movidas,Turno,D1,Alfa1,Beta1,nil,[Mv,Val]),!.
%no puede jugar
alpha_beta(D,_,_,_,_,Val):-
  evalua_1_2Js(V1),
  Val is -V1-2-D.

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

%alfabeta
ia_minimax_evalua(Js,Movidas,[Mano|Manos],ManoIA,Resp,D):-
  retractall(jugador_ficha_p(_,_)),
  retractall(turno_p(_,_,_,_,_)),
  asignar_fichas_p(Js,ManoIA),
  jugador_oculto(Jo),
  asignar_fichas_p(Jo,Mano),
  ultimo_turno(UltT),
  T1 is UltT+1,

  %nl,writeln(['  Mano siendo analizada: ',Mano]),

  %time()
  evaluate_and_choose_ab(Movidas,T1,D,-100,100,_,[Mv,Val]),

  %writeln(['    Movida: ',Mv,' Val: ',Val]),

  ia_minimax_cambia_valor_movida(Mv,Val),
  ia_minimax_evalua(Js,Movidas,Manos,ManoIA,Resp,D),!.
  %acaba alfabeta


ia_minimax_evalua(_,_,[],_,Resp,_):-
  findall(X,movida_minmax(_,X),ValMovidas),
  max_list(ValMovidas,MaxVal),
  movida_minmax(Resp,MaxVal),!.

ia_minimax_juega(Js,Fi,La):-
  ia_minimax(Js),
  ficha(Fi),
  lista_fichas_jugador(Fichas,Js),
  member(Fi,Fichas),
  jugar_turno(Js,Fi,La),!.

%esta de abajo juega una ficha aleatoria cuando es el primer turno
%se usa para las pruebas.
ia_minimax_juega_automatico(Js):-
  ultimo_turno(UltT),
  UltT = 0,
  random(N),
  E is floor(N*7)+1,
  lista_fichas_jugador(Mano,Js),
  nth1(E,Mano,Esc),
  ia_minimax_juega(Js,Esc,0).
ia_minimax_juega_automatico(Js):-
  %ultimo_turno(UltT),
  %UltT > 0,
  lista_fichas_jugador(ManoIA,Js),
  movidas_posibles(ManoIA,Movidas),
  length(Movidas,N),
  N=1,
  nth0(0,Movidas,Mov),
  nth0(0,Mov,R0),
  nth0(1,Mov,R1),
  ia_minimax_juega(Js,R1,R0),!.
ia_minimax_juega_automatico(Js):-
  %ultimo_turno(UltT),
  %UltT > 0,
  lista_fichas_jugador(ManoIA,Js),
  movidas_posibles(ManoIA,Movidas),
  length(Movidas,N),
  N=0,!,fail.

ia_minimax_juega_automatico(Js):-
  ultimo_turno(UltT),
  UltT > 0,
  time(conc_evalua_4(Js,Ri)),
  nth0(0,Ri,R0),
  nth0(1,Ri,R1),
  ia_minimax_juega(Js,R1,R0),!.

ia_minimax_come(Js,Fi):-
  jugador_asignar_ficha(Js,Fi).
ia_minimax_pasa(Js):-
  ia_minimax(Js),
  jugador_turno_valido(Js),
  pasar_turno(Js).



/*
 _____ _                        _ _
|_   _| |                      | (_)
  | | | |__  _ __ ___  __ _  __| |_ _ __   __ _
  | | | '_ \| '__/ _ \/ _` |/ _` | | '_ \ / _` |
  | | | | | | | |  __/ (_| | (_| | | | | | (_| |
  \_/ |_| |_|_|  \___|\__,_|\__,_|_|_| |_|\__, |
                                           __/ |
                                          |___/
                   _____ _
                  |_   _| |
                    | | | |__   ___
                    | | | '_ \ / _ \
                    | | | | | |  __/
                    \_/ |_| |_|\___|
___  ___      _ _   _ _   _                        _
|  \/  |     | | | (_) | | |                      | |
| .  . |_   _| | |_ _| |_| |__  _ __ ___  __ _  __| |____
| |\/| | | | | | __| | __| '_ \| '__/ _ \/ _` |/ _` |_  /
| |  | | |_| | | |_| | |_| | | | | |  __/ (_| | (_| |/ /
\_|  |_/\__,_|_|\__|_|\__|_| |_|_|  \___|\__,_|\__,_/___|
*/
break_here_plz.
conc_evalua_4(_,_):-
  findall(X,mano_posible_oculto(2,X),ManosOp),
  ManosOp = [], break_here_plz.

conc_evalua_4(Js,Resp):-
  retractall(movida_minmax(_,_)),
  lista_fichas_jugador(ManoIA,Js),
  movidas_posibles(ManoIA,Movidas),

  findall(X,mano_posible_oculto(2,X),ManosOp),
  length(ManosOp,Tam0),
  Tam0>100,
  write('Numero de manos posibles: '),
  writeln(Tam0),nl,
  random_permutation(ManosOp, ManosOpPer),
  primeros_n_ls(ManosOpPer,5000,LsFin),
  div_4(LsFin,L1,L2,L3,L4),
  concurrent(4,[ia_minimax_evalua_conc(Js,Movidas,L1,ManoIA,R1),
                ia_minimax_evalua_conc(Js,Movidas,L2,ManoIA,R2),
                ia_minimax_evalua_conc(Js,Movidas,L3,ManoIA,R3),
                ia_minimax_evalua_conc(Js,Movidas,L4,ManoIA,R4)],[]),
  cambia_valores_mvs_conc(R1),
  cambia_valores_mvs_conc(R2),
  cambia_valores_mvs_conc(R3),
  cambia_valores_mvs_conc(R4),
  ia_minimax_evalua(_,_,[],_,Resp,_),
  listing(movida_minmax),
  write('Resultado:'),writeln(Resp),nl,!.

conc_evalua_4(Js,Resp):-
  lista_fichas_jugador(ManoIA,Js),
  movidas_posibles(ManoIA,Movidas),

  findall(X,mano_posible_oculto(2,X),ManosOp),
  length(ManosOp,Tam0),
  Tam0>0,
  Tam0<101,
  write('Numero de manos posibles bajo: '),writeln(Tam0),
  writeln('Se va a usar la definicion alternativa (sin concurrencia D=10)'),
  ia_minimax_evalua(Js,Movidas,ManosOp,ManoIA,Resp,10),
  listing(movida_minmax),
  write('Resultado:'),writeln(Resp),nl,!.


cambia_valores_mvs_conc([Hi|Ls]):-
  nth0(0,Hi,H0),
  nth0(1,Hi,H1),
  ia_minimax_cambia_valor_movida(H0,H1),
  cambia_valores_mvs_conc(Ls).
cambia_valores_mvs_conc([]).

ia_minimax_evalua_conc(Js,Movidas,L1,ManoIA,LsMovs):-
  ia_minimax_evalua(Js,Movidas,L1,ManoIA,_,4),
  ls_movidas_minmax(LsMovs).

ls_movidas_minmax(Ls):-
  findall(X,movida_minmax(X,_),L1),
  findall(Y,movida_minmax(_,Y),L2),
  zip_2_ls(L1,L2,Ls).
zip_2_ls([X|Xl],[Y|Yl],[[X,Y]|Ls]):-
  zip_2_ls(Xl,Yl,Ls).
zip_2_ls([],[],[]).

primeros_n_ls([X1|L1],N,[X1|L2]):-
  N > 0,
  M is N-1,
  primeros_n_ls(L1,M,L2),!.
primeros_n_ls([],_,[]):-!.
primeros_n_ls(_,0,[]):-!.


div_4(L,A,B,C,D) :-
  div(L,L1,L2),
  div(L1,A,B),
  div(L2,C,D).
div_3(L, A, B, C):-
  length(L, N),
  T is N div 3,
  length(A, T),
  length(B, T),
  length(C, T).

div(L, A, B) :-
  length(L, N),
  Half is N div 2,
  length(A, Half),
  length(B, Half),
  append(A, B, L),!.
div(L, A, B) :-
  length(L, N),
  H1 is (N div 2) + 1,
  H2 is H1-1,
  length(A, H1),
  length(B, H2),
  append(A, B, L).
