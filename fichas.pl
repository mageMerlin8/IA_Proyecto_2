/*
______ _      _
|  ___(_)    | |
| |_   _  ___| |__   __ _ ___
|  _| | |/ __| '_ \ / _` / __|
| |   | | (__| | | | (_| \__ \
\_|   |_|\___|_| |_|\__,_|___/
*/

%:- module(fichas, [ficha/1, lista_fichas/1]).
/*
ficha([X|Y]) :- se definen los numeros a usar en las fichas.
                Despues se da una definicion de las fichas donde
                cada elemento es uno de los numeros y el primer numero
                es mayor o igual. Esto es para evitar que se repitan fichas:
                ficha([6|5]) es una ficha valida y ficha([5|6]) no lo es.
                Ademas se define ficha(0) para representar cuando algun jugador
                haya pasado.
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

lista_fichas(Ls) :-
  setof(X,ficha(X),Ls0),
  delete(Ls0, 0, Ls).

lista_fichas_jugadas(Ls):-
  findall(X, turno(X,_,_,_,_), Ls).

lista_fichas_jugador(Ls, Js):-
  jugador(Js),
  setof(X, jugador_ficha(Js,X), Ls),!.
lista_fichas_jugador([], _).
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
   ___
  |_  |
    | |_   _  ___  __ _  ___
    | | | | |/ _ \/ _` |/ _ \
/\__/ / |_| |  __/ (_| | (_) |
\____/ \__,_|\___|\__, |\___/
                   __/ |
                  |___/
*/
/*
:- module(juego, [jugador_ficha/2,
                  jugador_ficha_valida/2,
                  asignar_jugadores/1,
                  turno/4,
                  jugador/1,
                  lado/1,
                  jugador_asignar_ficha/2]).
*/
:- dynamic
      jugador_ficha/2,
      turno/5,
      jugador/1,
      max_jugadores/1,
      ia_basico/1.

asignar_jugadores(0) :-!.
asignar_jugadores(N) :-
  N =< 4,
  (max_jugadores(_),!;asserta(max_jugadores(N))),
  asserta(jugador(N)),
  M is N-1,
  asignar_jugadores(M).

/*
jugador_ficha(J,F) :- indica que la ficha F le pertenece a jugador J.
*/
jugador_ficha(0,0).
jugador_ficha_valida(Js,Fi):-
  jugador(Js),
  ficha(Fi),!.
/*
jugador_asignar_ficha(J,F) :- Le asigna la ficha F al jugador J. Checa que sea
                              valida la asignacion (el jugador y la ficha sean
                              validos y tambien que no se haya asignado esa ficha
                              a ese jugador).
*/
jugador_asignar_ficha(Js,Ficha) :-
  jugador_ficha_valida(Js,Ficha),
  (setof(X, jugador_ficha(Js,X), ListaFichasAsignadas),!;
  ListaFichasAsignadas = []),
  \+member(Ficha, ListaFichasAsignadas),
  asserta(jugador_ficha(Js,Ficha)),
  write('Ficha Asignada: J-'),write(Js),write(' F-'),write(Ficha),nl.

/*
turno(Ficha, jugador, lado, numTurno, ladoLibre)
*/
lado(1).
lado(2).
lado(0).
turno(0,0,0,0,0).
turno([6,6],1,1,1,0).
ultimo_turno(T) :-
  findall(X, turno(_,_,_,X,_), Ls),
  max_list(Ls, T).
ultimo_jugador(J) :-
  ultimo_turno(T),
  turno(_,J,_,T,_).

turno_ficha(Ficha, Turno):-
  turno(Ficha,_,_,Turno,_),!.
turno_ficha(F,_):-
  write('La ficha '),
  write(F),
  write(' no se ha jugado').

ultima_ficha_lado(Lado, Ficha) :-
  ultimo_turno(T),
  (turno(Ficha,_,Lado,T,_),!;
  (T1 is T-1,ultima_ficha_lado(Lado, Ficha, T1))
  ).
ultima_ficha_lado(_, Ficha, -1):-
  turno(Ficha,_,_,1,_),!.
ultima_ficha_lado(Lado, Ficha, T) :-
  (turno(Ficha,_,Lado,T,_),!);
  (T1 is T-1,ultima_ficha_lado(Lado, Ficha, T1)).

ficha_turno_valida(_,_,0):-
  ultimo_turno(Ult), Ult is 0,!.
ficha_turno_valida(Ficha, Lado, LL):-
  ultima_ficha_lado(Lado, UltimaFicha),
  turno_ficha(UltimaFicha, UltT),
  turno(UltimaFicha,_,_,UltT,UltLibre),

  UltLibre is 0,
  nth1(Lado,UltimaFicha,NumeroUltimaFicha),
  (
  (nth1(1,Ficha,NumeroUltimaFicha), LL is 2,!);
  (nth1(2,Ficha,NumeroUltimaFicha), LL is 1,!)
  ).
ficha_turno_valida(Ficha, Lado, LL) :-
  %ficha y lado
  ultima_ficha_lado(Lado, UltimaFicha),
  turno_ficha(UltimaFicha, UltT),
  turno(UltimaFicha, _, Lado, UltT, UltLibre),

  nth1(UltLibre, UltimaFicha, NumeroLlibre),
  %member(NumeroLlibre, Ficha)
  (
  (nth1(1,Ficha,NumeroLlibre), LL is 2,!);
  (nth1(2,Ficha,NumeroLlibre), LL is 1,!)
  ).

jugador_turno_valido(_) :-
  ultimo_turno(Ult),
  Ult is 0,!.
jugador_turno_valido(Jugador):-
  max_jugadores(MaxJs),
  ultimo_jugador(UltJs),
  %jugador
  ((Jugador is UltJs + 1,!);
  (Jugador is (UltJs + 1) rem MaxJs,!)).

jugar_turno(Jugador, Ficha, Lado) :-
  %Validar turno
  jugador(Jugador),
  ficha(Ficha),
  lado(Lado),
  jugador_turno_valido(Jugador),
  ficha_turno_valida(Ficha, Lado, LL),
  retract(jugador_ficha(Jugador,Ficha)),
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
 _____      _       _ _                       _        ______           _
|_   _|    | |     | (_)                     (_)       | ___ \         (_)
  | | _ __ | |_ ___| |_  __ _  ___ _ __   ___ _  __ _  | |_/ / __ _ ___ _  ___ __ _
  | || '_ \| __/ _ \ | |/ _` |/ _ \ '_ \ / __| |/ _` | | ___ \/ _` / __| |/ __/ _` |
 _| || | | | ||  __/ | | (_| |  __/ | | | (__| | (_| | | |_/ / (_| \__ \ | (_| (_| |
 \___/_| |_|\__\___|_|_|\__, |\___|_| |_|\___|_|\__,_| \____/ \__,_|___/_|\___\__,_|
                         __/ |
                        |___/
*/
%checar si ll es 0 antes
ia_basico_juega(Js, Lado):-
  lista_fichas_jugador(ListaFichas, Js),
  ultima_ficha_lado(Lado, UltF),
  turno(UltF, _, _, UltT, LL),
  LL is 0,
  nth1(1, UltF, NumeroLibre),
  ( (member([NumeroLibre,X],ListaFichas), FichaAJugar = [NumeroLibre, X],!);
  %LadoLibreJugador = 2,!);
  (member([X,NumeroLibre],ListaFichas), FichaAJugar = [X,NumeroLibre],!) ),
  %LadoLibreJugador = 1,!) ),
  jugar_turno(Js, FichaAJugar, Lado),!.
ia_basico_juega(Js, Lado):-
  lista_fichas_jugador(ListaFichas, Js),
  ultima_ficha_lado(Lado, UltF),
  turno(UltF, _, _, UltT, LL),
  LL is 0,
  nth1(2, UltF, NumeroLibre),
  ( (member([NumeroLibre,X],ListaFichas), FichaAJugar = [NumeroLibre, X],!);
  %LadoLibreJugador = 2,!);
  (member([X,NumeroLibre],ListaFichas), FichaAJugar = [X,NumeroLibre],!) ),
  %LadoLibreJugador = 1,!) ),
  jugar_turno(Js, FichaAJugar, Lado),!.

ia_basico_juega(Js, Lado):-
  lista_fichas_jugador(ListaFichas, Js),
  ultima_ficha_lado(Lado, UltF),
  turno(UltF, _, _, UltT, LL),
  nth1(LL, UltF, NumeroLibre),
  ( (member([NumeroLibre,X],ListaFichas), FichaAJugar = [NumeroLibre, X],!);
  %LadoLibreJugador = 2,!);
  (member([X,NumeroLibre],ListaFichas), FichaAJugar = [X,NumeroLibre],!) ),
  %LadoLibreJugador = 1,!) ),
  jugar_turno(Js, FichaAJugar, Lado),!.
ia_basico_juega(Js):-
  \+jugador_turno_valido(Js),
  write('No es turno de este jugador.'),!,fail.
ia_basico_juega(Js):-
  jugador_turno_valido(Js),
  ia_basico(Js),
  (
  (ia_basico_juega(Js, 1),!);
  (ia_basico_juega(Js, 2),!);
  (come(Js), ia_basico_juega(Js))
  ).
come(Js):-
  asigna_piezas_aleatorio(Js,1).
asigna_piezas_aleatorio(Js,0):-!.
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
  asigna_piezas_aleatorio(Js, 7).

crea_ia_basico(Js):-
  jugador(Js),
  asserta(ia_basico(Js)),
  write('IA #'),write(Js),write(' Basico creado '),
  asigna_piezas_aleatorio(Js),nl.





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
