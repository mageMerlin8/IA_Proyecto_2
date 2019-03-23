/*
______ _      _
|  ___(_)    | |
| |_   _  ___| |__   __ _ ___
|  _| | |/ __| '_ \ / _` / __|
| |   | | (__| | | | (_| \__ \
\_|   |_|\___|_| |_|\__,_|___/

Aqui se definen las fichas y funciones de utilidad para conseguir listas de
interes como fichas disponibles o fichas por jugador asi como funciones para asignar fichas a jugadores.

Variables:
  Empiezan con una letra minuscula que indica el uso correcto:
    i: indica que la variable debe usarse como entrada
    o: indica que la variable debe usarse como salida
    u: indica que la variable puede usarse tanto como de salida como de entrada
  Seguidas de una letra mayuscula y cero o mas letras mayusculas o minusculas o
  numeros.
  ej: iJ0 - puede usarse para referirse a una variable de entrada para indicar
            un numero de jugador
*/

%:- module(fichas, [ficha/1, lista_fichas/1]).
/*
ficha([X|Y]) :-
    Se definen los numeros a usar en las fichas. Despues se da una definicion
    de las fichas donde cada elemento es uno de los numeros y el primer numero
    es mayor o igual. Esto es para evitar que se repitan fichas: ficha([6|5])
    es una ficha valida y ficha([5|6]) no lo es. Ademas se define ficha(0) para
    representar cuando algun jugador haya pasado.
*/
numero(6).
numero(5).
numero(4).
numero(3).
numero(2).
numero(1).
numero(0).

ficha([X,Y]):-
  numero(X),
  numero(Y),
  Y =< X.
ficha(0).
/*
lista_fichas(oL) :- Funcion que regresa una lista de todas las fichas. Busca
                    usando la definicion de ficha/1.
*/
lista_fichas(Ls) :-
  setof(X,ficha(X),Ls0),
  delete(Ls0, 0, Ls).
/*
lista_fichas_jugadas(oL) :- Funcion que regresa la lista de todas las fichas que
                            han sido jugadas.
*/
lista_fichas_jugadas(Ls):-
  findall(X, turno(X,_,_,_,_), Ls).
/*
lista_fichas_jugador(oL, iJ) :-
    Funcion que regresa la mano (lista de fichas de un jugador sin tomar en
    cuenta las que ha jugado) de algun jugador. iJ es el numero del jugador y
    oL es la mano del jugador.
*/
lista_fichas_jugador(Ls, Js):-
  jugador(Js),
  setof(X, jugador_ficha(Js,X), Ls),!.
lista_fichas_jugador([], _).
/*
lista_fichas_disponibles(o) :- Funcion que regresa una lista de todas las
                               fichas que no han sido jugadas y no pertenecen a
                               algun jugador.
*/
lista_fichas_disponibles(Ls):-
    %asumiendo que todas las fichas son asignadas publicamente.
    lista_fichas(Todas),
    lista_fichas_jugadas(Jugadas),
    lista_fichas_jugador(J1,1),
    lista_fichas_jugador(J2,2),
    lista_fichas_jugador(J3,3),
    lista_fichas_jugador(J4,4),
    union(Jugadas, J1, NoDisp0),
    union(NoDisp0, J2, NoDisp1),
    union(NoDisp1, J3, NoDisp2),
    union(NoDisp2, J4, NoDispFin),
    subtract(Todas, NoDispFin, Ls).

/*
come_aleatorio(i) :- Funcion que asigna una ficha de la lista de disponibles
                     a algun jugador. Toma como parametro el numero del jugador
                     y escribe a la consola un mensaje de la ficha asignada.
*/
come_aleatorio(_):-
  lista_fichas_disponibles(Disp),
  Disp = [],!,fail.
come_aleatorio(Js):-
  asigna_piezas_aleatorio(Js,1).

/*
come(iJ, iF) :- Intenta asignar la ficha iF al jugador iJ. Escribe mensaje de
                error a la consola.
*/
come(Js, Fi):-
  jugador_asignar_ficha(Js, Fi).
come(Js,Fi):-
  write('Jugador #'),write(Js),
  write('no puede comer la ficha '),write(Fi),!.

/*
asigna_piezas_aleatorio(i, i) :- Toma como parametros un numero de jugador y un
                                 numero de fichas. Asigna aleatoriamente el
                                 numero de fichas al jugador.
asigna_piezas_aleatorio(i) :- Asigna 7 piezas aleatoriamente al jugador indicado
                              por el primer parametro.
*/
asigna_piezas_aleatorio(_,0):-!.
asigna_piezas_aleatorio(Js, Num):-
  random(Rand),
  lista_fichas_disponibles(Disp),
  length(Disp, TotDisp),
  Escoger is floor(Rand * TotDisp),
  nth0(Escoger, Disp, Ficha),
  jugador_asignar_ficha(Js, Ficha),
  Num2 is Num - 1,
  asigna_piezas_aleatorio(Js, Num2).
asigna_piezas_aleatorio(Js):-
  num_piezas_ini(X),
  asigna_piezas_aleatorio(Js, X).
/*
escribe_fichas(i) :- Toma como parametro una lista de fichas y la escribe a la
                     consola con el indice de la lista (empezando en 1).
*/
escribe_fichas(Ls):-
  escribe_fichas(Ls,1).
escribe_fichas([],_):- nl,!.
escribe_fichas([F1|Fs],Num):-
  write(' '),write(Num),write(':'),write(F1),
  M is Num + 1,
  escribe_fichas(Fs, M).
/*
   ___
  |_  |
    | |_   _  ___  __ _  ___
    | | | | |/ _ \/ _` |/ _ \
/\__/ / |_| |  __/ (_| | (_) |
\____/ \__,_|\___|\__, |\___/
                   __/ |
                  |___/
Aqui se define todo lo pertinente a un juego. Se le da estructura al domino y se
crean funciones de utilidad para preguntar informacion importante del juego.


*/
/*
Hechos dinamicos:
----------------------------
Los siguientes hechos dinamicos se utilizan a lo lago del resto del codigo ya
que son responsables de guardar la informacion de cada juego. Es importante
borrar todos estos hechos cuando se inicie un juego nuevo. Esto se logra con el
predicado nuevo_juego/0.

jugador_ficha(J,F) :- Hecho dinamico que indica cuando la ficha F le pertenece
                      a jugador J.

turno(ficha, jugador, lado, num_turno, lado_libre) :-
    Hecho dinamico que se usa para guardar toda la informacion de los turnos:
      ficha: la ficha jugada,
      jugador: el numero del jugador que jugo el turno,
      lado: el lado de la cola donde se jugo,
      num_turno: el numero del turno que se jugo,
      lado_libre: el lado libre de la ficha que se jugo (indice en la lista de
                  la ficha empezando en 1).
    Ademas se debe definir un turno(0,0,0,0,0) como caso base de algunas
    funciones.
    Se define un turno de un jugador que pasa de la forma turno(0,J,0,N,0).

max_jugadores(num) :- Hecho dinamico que indica el numero maximo de jugadores.

jugador(num) :- Hecho dinamico que indica que existe un jugador con en numero
                num. Se usa para saber quienes son jugadores en un juego. Se
                deben asignar todos los jugadores al principio de cada juego.


ia_basico(num) :- Hecho dinamico que indica que num es el numero de un jugador
                  autonomo tipo 'ia_basico'. Este es un agente autonomo que
                  decide sus jugadas de forma algoritmica (sin inteligencia).
                  Sirve para probar el juego y otros agentes mas inteligentes.

jugador_persona(num) :- Hecho dinamico que indica que num es el numero de un
                        jugador humano. La definicion de persona_juega/1 deja
                        que un usuario tome la decision de la jugada. Se debe
                        tomar en cuenta que no bloquea cosas como comer cuando
                        si  es posible jugar y por lo tanto el jugador humano
                        debe tomar en cuenta las reglas del juego al decidir
                        por un agente tipo 'jugador_persona'.

jugador_oculto(num) :- Hecho dinamico que indica que num es el numero de un
                       jugador 'oculto'. Un jugador oculto puede jugar
                       cualquier ficha que no haya sido jugada antes y que no
                       pertenezca a algun jugador.

oculto_comio(jugador,turno) :- Hecho dinamico que indica cuando el jugador
                               comio una ficha durante el turno. Solamente se
                               debe usar para losjugadores ocultos.
jugador_oculto_no_tiene(jugador, numero):-
      Hecho dinamico que indica cuando se sabe que el jugador oculto no tiene
      el numero.
*/
:- dynamic
      jugador_ficha/2,
      turno/5,
      max_jugadores/1,
      jugador/1,
      ia_basico/1,
      jugador_persona/1,
      jugador_oculto/1,
      oculto_comio/2,
      jugador_oculto_no_tiene/2.
%se define el numero inicial de fichas de cada jugador
num_piezas_ini(7).
/*
lado(num) :- hecho que define los lados de la cola. lado(0) esta reservado
             unicamente para la primera ficha de la cola porque esa es colocada
             'en los dos lados al mismo tiempo'. A partir de la primera lado(1)
             es el indice 1 de la ficha colocada primero y lado(2) el indice 2
             de la ficha colocada.
*/
lado(1).
lado(2).
lado(0).

/*
nuevo_juego :- Predicado para empezar un juego nuevo. Restaura todos los
               predicados dinamicos a los valores iniciales.
*/
% nuevo_juego :-
%   retractall(jugador_ficha(_,_)),
%   retractall(turno(_,_,_,_,_)),
%   retractall(oculto_comio(_,_)),
%   retractall(jugador(_)),
%   retractall(max_jugadores(_)),
%   retractall(ia_basico(_)),
%   retractall(jugador_persona(_)),
%   retractall(jugador_oculto(_)),
%   assert(turno(0,0,0,0,0)).
/*
juega(iJ) :- intenta hacer jugar al siguiente jugador. Llama todas las Funciones
             *_juega/1 hasta que una sea verdadera. Es falsa si no es posible hacer jugar a ese jugador.
*/
juega(Js):-
  jugador(Js),
  jugador_turno_valido(Js),
  (
  (ia_basico_juega(Js),!);
  (persona_juega(Js),!)
  ).


/*
ganador(oJ) :- Funcion que busca un ganador. oJ es el numero del ganador o -1
               en caso de empate. Ademas escribe un mensaje a la consola.
*/
ganador(Js):-
  jugador(Js),
  lista_fichas_jugador(Ls,Js),
  Ls = [],
  write('Ganador: '),write(Js),!.
ganador(-1):-
  %write('Empate!'),
  fail.

/*
asignar_jugadores(iN) :- Funcion que crea iN (numero) jugadores. Se utiliza al
                         principio del juego para asignar iN jugadores
*/
asignar_jugadores(0) :-!.
asignar_jugadores(N) :-
  N =< 4,
  (max_jugadores(_),!;asserta(max_jugadores(N))),
  asserta(jugador(N)),
  M is N-1,
  asignar_jugadores(M).

/*
jugador_ficha_valida(iJ,iF) :- Predicado que es verdadero si es posible asignar
                               una ficha (iF) dado un jugador (iJ).
*/
jugador_ficha_valida(Js,Fi):-
  jugador(Js),
  ficha(Fi),
  lista_fichas_disponibles(Disp),
  member(Fi, Disp),!.

/*
jugador_asignar_ficha(J,F) :- Le asigna la ficha F al jugador J. Checa que sea
                              valida la asignacion (el jugador y la ficha sean
                              validos y tambien que no se haya asignado esa ficha a ese jugador).
*/
jugador_asignar_ficha(Js,Ficha) :-
  jugador_ficha_valida(Js,Ficha),
  asserta(jugador_ficha(Js,Ficha)),
  write('Ficha Asignada: J-'),write(Js),write(' F-'),write(Ficha),nl.


/*
ultimo_turno(oT) :- Una funcion que regresa el numero del ultimo turno (oT).
*/
ultimo_turno(T) :-
  findall(X, turno(_,_,_,X,_), Ls),
  max_list(Ls, T).

/*
ultimo_jugador(oJ) :- Regresa el numero del ultimo jugador (oJ).
*/
ultimo_jugador(J) :-
  ultimo_turno(T),
  turno(_,J,_,T,_).
  /*,
  \+Fi = 0,!.
ultimo_jugador(J) :-
  ultimo_turno(T),
  T1 is T-1,
  turno(_,J,_,T1,_).*/
/*
ultima_ficha_lado(iL, oF) :- Funcion que regresa (oF) la ultima ficha que se
                             jugo en el lado iL.
*/
ultima_ficha_lado(Lado,Ficha):-
  findall(X,turno(_,_,Lado,X,_),Ls),
  max_list(Ls,T),
  turno(Ficha,_,_,T,_),!.
ultima_ficha_lado(_,Ficha):-
  findall(X,turno(X,_,0,_,_),Ls),
  member(Ficha,Ls),
  ficha(Ficha).
  /*
ultima_ficha_lado(Lado, Ficha) :-
  ultimo_turno(T),
  (turno(Ficha,_,Lado,T,_),!;
  (T1 is T-1,ultima_ficha_lado(Lado, Ficha, T1))
  ).
%Funciones auxiliares
ultima_ficha_lado(_, Ficha, -1):-
  turno(Ficha,_,_,1,_),!.
ultima_ficha_lado(Lado, Ficha, T) :-
  (turno(Ficha,_,Lado,T,_),!);
  (T1 is T-1,ultima_ficha_lado(Lado, Ficha, T1)).*/
/*
ficha_num_libre(iF,oN) :- oN es el numero que esta libre de la ficha iF. iF es
                          una ficha jugada.
*/
ficha_num_libre([X,Y],X):-
  turno([X,Y],_,_,_,1),!.
ficha_num_libre([X,Y],Y):-
  turno([X,Y],_,_,_,2),!.
ficha_num_libre([X,Y],X):-
  turno([X,Y],_,0,1,0),
  \+turno(_,_,1,_,_).
ficha_num_libre([X,Y],Y):-
  turno([X,Y],_,0,1,0),
  \+turno(_,_,2,_,_).


/*
ficha_turno_valida(iF, iL, uLL) :- Funcion para validar un turno. Toma una ficha
                                   iF y un lado iL. Ademas uLL se puede usar
                                   para 'intentar un turno' o para ver si la
                                   ficha cabe en algun lugar y ver que lado
                                   queda libre.
*/
ficha_turno_valida(_,_,0):-
  ultimo_turno(Ult), Ult is 0,!.
ficha_turno_valida(Ficha, Lado, LL):-
  ultima_ficha_lado(Lado, UltimaFicha),
  turno(UltimaFicha,_,_,_,UltLibre),
  UltLibre is 0,!,
  nth1(Lado,UltimaFicha,NumeroUltimaFicha),
  (
  (nth1(1,Ficha,NumeroUltimaFicha), LL is 2,!);
  (nth1(2,Ficha,NumeroUltimaFicha), LL is 1,!)
  ).
ficha_turno_valida(Ficha, Lado, 1):-
  ultima_ficha_lado(Lado,Uf),
  ficha_num_libre(Uf,Ul),
  nth1(2,Ficha,Ul),!.
ficha_turno_valida(Ficha, Lado, 2) :-
  ultima_ficha_lado(Lado,Uf),
  ficha_num_libre(Uf,Ul),
  nth1(1,Ficha,Ul),!.

 % %ficha y lado
  % ultima_ficha_lado(Lado, UltimaFicha),
  % turno(UltimaFicha, _, _, _, UltLibre),
  %
  % nth1(UltLibre, UltimaFicha, NumeroLlibre),
  % %member(NumeroLlibre, Ficha)
  % (
  % (nth1(1,Ficha,NumeroLlibre), LL is 2,!);
  % (nth1(2,Ficha,NumeroLlibre), LL is 1,!)
  % ).

/*
jugador_turno_valido(uJ) :- Predicado que es verdadero cuando es turno del
                            jugador uJ.
*/
jugador_turno_valido(_) :-
  ultimo_turno(Ult),
  Ult is 0,!.
jugador_turno_valido(Jugador):-
  max_jugadores(MaxJs),
  ultimo_jugador(UltJs),
  %jugador
  ((Jugador is UltJs + 1, jugador(Jugador),!);
  (Jugador is (UltJs + 1) rem MaxJs, jugador(Jugador), !)).

/*
jugar_turno(iJ, iF, uL) :-
    Hace todos los pasos necesarios para jugar un turno de parte de un jugador. Valida toda la informacion del turno:
        iJ - numero del jugador,
        iF - ficha a jugar,
        uL - lado a jugar (si uL no es dado se asigna automaticamente).
    Ademas acerta el turno a la base dinamica.
*/
jugar_turno(Jugador, Ficha, Lado) :-
  %Validar turno
  jugador(Jugador),
  ficha(Ficha),
  lado(Lado),
  jugador_turno_valido(Jugador),
  ficha_turno_valida(Ficha, Lado, LL),
  %para que funcione con jugadores ocultos
  (retract(jugador_ficha(Jugador,Ficha));true),
  %folio
  ultimo_turno(UltT),
  NumTurno is UltT+1,

  %acertar y avisar
  asserta(turno(Ficha, Jugador, Lado, NumTurno, LL)),
  write('Turno: '),write(NumTurno),nl,
  write('  Jugador:  '),write(Jugador),nl,
  write('  Ficha:    '),write(Ficha),nl,
  write('  Lado:     '),write(Lado),nl,
  write('  LadoLibre:'),write(LL),nl.

/*
pasar_turno(iJ) :- Hace que el jugador iJ pase. Solamente se debe llamar cuando un jugador no puede ni comer ni poner ficha.
*/
pasar_turno(Js) :-
  jugador(Js),
  ultimo_turno(UltT),
  NumT is UltT + 1,
  asserta(turno(0,Js,0,NumT,0)),
  write('Turno: '),write(NumT),nl,
  write('  Jugador:  '),write(Js),nl,
  writeln('  El jugador pasa su turno.').
/*
______
| ___ \
| |_/ /__ _ __ ___  ___  _ __   __ _
|  __/ _ \ '__/ __|/ _ \| '_ \ / _` |
| | |  __/ |  \__ \ (_) | | | | (_| |
\_|  \___|_|  |___/\___/|_| |_|\__,_|

Se definen toda la funcionalidad para un jugador humano. Deja que el humano tome
todas las decisiones entonces es importante poner atencion al hacer las jugadas.
Por la definicion del juego no es posible hacer jugadas incorrectas pero si es
posible comer cuando no es necesario.
*/
/*
crea_persona(iJ) :- Funcion para crear un jugador humano. Le asigna 7 piezas de
                    manera aleatoria y lo escribe a la base de datos y muestra
                    un mensaje.
*/
crea_persona(Js) :-
  jugador(Js),
  assert(jugador_persona(Js)),
  write('Jugador humano creado #'),
  write(Js),nl,
  asigna_piezas_aleatorio(Js).
/*
persona_juega(iJ) :- Funcion para hacer jugar al jugador humano iJ. Entra en un
                     ciclo y le da mensajes al usuario para tomar las decisiones
                     del turno.
*/
persona_juega(Js):-
  jugador_persona(Js),
  lista_fichas_jugador(Lista, Js),
  %no tiene fichas entonces gano
  Lista = [],!,fail.
persona_juega(Js):-
  jugador_persona(Js),
  %no le toca
  \+jugador_turno_valido(Js),!,fail.
persona_juega(Js):-
  jugador_persona(Js),
  repite,
  lista_fichas_jugador(Fichas,Js),
  writeln('Escoge una ficha, come o pasa:'),
  writeln('-1:come '),
  writeln('-2:pasa '),
  escribe_fichas(Fichas),
  read(Fi),nl,
  (
  %comer
  (Fi is -1, come_aleatorio(Js), fail);
  %pasar
  (Fi is -2, pasar_turno(Js), !);
  %escoger lado
  (nth1(Fi, Fichas, Ficha),
   write('Seleccion: '),writeln(Ficha),
   writeln('De que lado quieres jugar (1 o 2)?'),
   read(Lado));
  %ficha invalida
  (write('No existe la ficha que seleccionaste'),nl,fail)
  ),
  %intentar turno
  (
  (jugar_turno(Js,Ficha,Lado),!);
  (write('ficha invalida. intenta de nuevo'),fail)
  ).
repite.
repite:-repite.
/*
 _____            _ _
|  _  |          | | |
| | | | ___ _   _| | |_ ___
| | | |/ __| | | | | __/ _ \
\ \_/ / (__| |_| | | || (_) |
 \___/ \___|\__,_|_|\__\___/

Aqui se define la funcionalidad de un jugador 'oculto'. De este jugador no se sabe sus fichas. Se definen funciones para contar sus fichas.
*/

/*
crea_jugador_oculto(iN) :-
*/
crea_jugador_oculto(Js) :-
  jugador(Js),
  assert(jugador_oculto(Js)),
  write('Jugador oculto creado #'),
  write(Js),nl.
jugador_oculto_juega(Js, [F1,F2], La) :-
  jugador_oculto(Js),
  jugador_turno_valido(Js),
  lista_fichas_disponibles(Disp),
  member([F1,F2],Disp),
  jugar_turno(Js, [F1,F2], La),!,
  retractall(jugador_oculto_no_tiene(Js,F1)),
  retractall(jugador_oculto_no_tiene(Js,F2)).

jugador_oculto_pasa(Js):-
  jugador_oculto(Js),
  jugador_turno_valido(Js),
  pasar_turno(Js).

jugador_oculto_come(Js):-
  jugador_oculto(Js),
  jugador_turno_valido(Js),
  ultimo_turno(UltT),
  %acertar que comio
  asserta(oculto_comio(Js,UltT)),
  %cuando come, dejamos de saber cuales no tiene
  retractall(jugador_oculto_no_tiene(Js,_)),
  %pero si sabemos que no tiene las que no pudo jugar
  ultima_ficha_lado(1,UltF1),
  ultima_ficha_lado(2,UltF2),
  ficha_num_libre(UltF1,X1),
  ficha_num_libre(UltF2,X2),
  asserta(jugador_oculto_no_tiene(Js,X1)),
  asserta(jugador_oculto_no_tiene(Js,X2)).


mano_posible_oculto(Js,Comb):-
  lista_fichas_disponibles_oculto(Js,Disp),
  numero_fichas_jugador_oculto(Js,Tam),
  combinacion(Tam, Disp, Comb).
  %permutation(Ls,Comb).
/*
combinacion(tamanio,lista,resp)
*/
combinacion(0,_,[]).
combinacion(N,[X|T],[X|Comb]) :-
    N>0,
    N1 is N-1,
    combinacion(N1,T,Comb).
combinacion(N,[_|T],Comb) :-
    N>0,
    combinacion(N,T,Comb).

lista_fichas_disponibles_oculto(Js,Ls):-
  lista_fichas_disponibles(Disp),
  (
  (setof(X,jugador_oculto_no_tiene(Js,X), NumsNoTiene),!);
  NumsNoTiene = []
  ),
  lista_fichas_sin_lista(FiNoTiene,NumsNoTiene),
  subtract(Disp, FiNoTiene, Ls).

lista_fichas_sin_lista([],[]).
lista_fichas_sin_lista(Resp, [Num|Lnumeros]):-
  lista_fichas_sin_lista(FichasP,Lnumeros),
  setof(X, ficha_con(X,Num), FichasN),
  union(FichasP,FichasN,Resp).
ficha_con([X|Y],Num):-
  ficha([X|Y]),
  X =:= Num.
ficha_con([X,Y],Num):-
  ficha([X|Y]),
  Y =:= Num.


numero_fichas_jugador_oculto(Js, Num) :-
  jugador_oculto(Js),
  num_piezas_ini(Ini),
  ((findall(X,oculto_comio(Js,X),ListaComidas),!);
  ListaComidas = []),
  length(ListaComidas, NumComidas),
  ((findall(X,turno(X,Js,_,_,_), LsFichasJugadas),!);
  LsFichasJugadas = []),
  length(LsFichasJugadas, NumJugadas),
  Num is Ini + NumComidas - NumJugadas.

/*
 _____  ___   ______           _
|_   _|/ _ \  | ___ \         (_)
  | | / /_\ \ | |_/ / __ _ ___ _  ___ ___
  | | |  _  | | ___ \/ _` / __| |/ __/ _ \
 _| |_| | | | | |_/ / (_| \__ \ | (_| (_) |
 \___/\_| |_/ \____/ \__,_|___/_|\___\___/
Aqui se define todo el funcionamiento del primer agente autonomo. Toma la decision de la primera ficha que encuentre que pueda jugar.
*/

/*
crea_ia_basico(iJ) :- Funcion para crear un jugador tipo 'ia_basico'. Al igual
                      que crea_persona/1, le asigna 7 fichas de manera aleatoria y muestra un mensaje.
*/
crea_ia_basico(Js):-
  jugador(Js),
  asserta(ia_basico(Js)),
  write('IA #'),write(Js),write(' Basico creado '),
  asigna_piezas_aleatorio(Js),nl.

%Funciones auxiliares
ia_basico_juega(Js, Lado):-
  lista_fichas_jugador(ListaFichas, Js),
  ultima_ficha_lado(Lado, UltF),
  turno(UltF, _, _, _, LL),
  LL is 0,
  nth1(1, UltF, NumeroLibre),
  ( (member([NumeroLibre,X],ListaFichas), FichaAJugar = [NumeroLibre, X],!);
  (member([X,NumeroLibre],ListaFichas), FichaAJugar = [X,NumeroLibre],!) ),
  jugar_turno(Js, FichaAJugar, Lado),!.
ia_basico_juega(Js, Lado):-
  lista_fichas_jugador(ListaFichas, Js),
  ultima_ficha_lado(Lado, UltF),
  turno(UltF, _, _, _, LL),
  LL is 0,
  nth1(2, UltF, NumeroLibre),
  ( (member([NumeroLibre,X],ListaFichas), FichaAJugar = [NumeroLibre, X],!);
  (member([X,NumeroLibre],ListaFichas), FichaAJugar = [X,NumeroLibre],!) ),
  jugar_turno(Js, FichaAJugar, Lado),!.

ia_basico_juega(Js, Lado):-
  lista_fichas_jugador(ListaFichas, Js),
  ultima_ficha_lado(Lado, UltF),
  turno(UltF, _, _, _, LL),
  nth1(LL, UltF, NumeroLibre),
  ( (member([NumeroLibre,X],ListaFichas), FichaAJugar = [NumeroLibre, X],!);
  (member([X,NumeroLibre],ListaFichas), FichaAJugar = [X,NumeroLibre],!) ),
  jugar_turno(Js, FichaAJugar, Lado),!.

/*
ia_basico_juega(iJ) :- Funcion para hacer que el jugador tipo 'ia_basico'
                       juegue.
*/
ia_basico_juega(Js):-
  ia_basico(Js),
  \+jugador_turno_valido(Js),
  write('No es turno de este jugador.'),nl,!,fail.
ia_basico_juega(Js) :-
  ia_basico(Js),
  ultimo_turno(UltT),
  UltT is 0,
  lista_fichas_jugador(ListaF, Js),
  member([X,X], ListaF),
  jugar_turno(Js, [X,X], 0),!.
ia_basico_juega(Js) :-
  ia_basico(Js),
  ultimo_turno(UltT),
  UltT is 0,
  lista_fichas_jugador(ListaF, Js),
  member([X,Y], ListaF),
  jugar_turno(Js, [X,Y], 0),!.
ia_basico_juega(Js):-
  jugador_turno_valido(Js),
  ia_basico(Js),
  (
  (ia_basico_juega(Js, 1),!);
  (ia_basico_juega(Js, 2),!);
  (come_aleatorio(Js), ia_basico_juega(Js));
  (pasar_turno(Js),!)
  ).

/*
___  ___      _           _______                _
|  \/  |     (_)         / / ___ \              | |
| .  . | __ _ _ _ __    / /| |_/ / __ _   _  ___| |__   __ _ ___
| |\/| |/ _` | | '_ \  / / |  __/ '__| | | |/ _ \ '_ \ / _` / __|
| |  | | (_| | | | | |/ /  | |  | |  | |_| |  __/ |_) | (_| \__ \
\_|  |_/\__,_|_|_| |_/_/   \_|  |_|   \__,_|\___|_.__/ \__,_|___/
*/

%Funcionalidad general de las reglas del domino y funcionamiento de ia basico
/*
main :-
  write('Numero de Jugadores: 2(default)'),nl,
  %read(NumJs),
  asignar_jugadores(2),
  jugador_asignar_ficha(1,[6,6]),
  jugador_asignar_ficha(1,[6,0]),
  jugador_asignar_ficha(1,[6,1]),
  jugador_asignar_ficha(1,[6,2]),
  jugador_asignar_ficha(2,[4,0]),
  jugador_asignar_ficha(2,[4,1]),
  jugador_asignar_ficha(2,[4,2]),
  jugador_asignar_ficha(2,[6,4]),
  jugar_turno(1,[6,6],1).
  jugar_turno(2,[6,4],1),
  jugar_turno(1,[6,0],2),
  jugar_turno(2,[4,2],1),
  jugar_turno(1,[6,2],1),
  jugar_turno(2,[0,0],2),
  jugar_turno(1,[3,0],2),
  jugar_turno(2,[6,3],1),
  jugar_turno(1,[3,1],2),
  jugar_turno(2,[5,1],2),
  jugar_turno(1,[6,5],2).
*/

%Juego autonomo
/*
main :-
  write('Numero de Jugadores: 2(default)'),nl,
  asignar_jugadores(2),nl,
  juega_2_ias.
*/
%:-main.
