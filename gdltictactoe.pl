%para ejecutar ?-inicio,juego. (Run!)

%roles o y x
role(o,Estado).
role(x,_).

%estado inicial
init(cell(1,1,6,b)).
init(cell(1,2,1,b)).
init(cell(1,3,2,b)).
init(cell(1,4,3,b)).
init(cell(1,5,6,b)).
init(cell(1,6,5,b)).
init(cell(2,1,4,b)).
init(cell(2,2,5,b)).
init(cell(2,3,3,b)).
init(cell(2,4,5,b)).
init(cell(2,5,2,b)).
init(cell(2,6,1,b)).
init(cell(3,1,5,b)).
init(cell(3,2,3,b)).
init(cell(3,3,6,b)).
init(cell(3,4,4,b)).
init(cell(3,5,5,b)).
init(cell(3,6,4,b)).
init(cell(4,1,1,b)).
init(cell(4,2,6,b)).
init(cell(4,3,1,b)).
init(cell(4,4,6,b)).
init(cell(4,5,4,b)).
init(cell(4,6,2,b)).
init(cell(5,1,3,b)).
init(cell(5,2,4,b)).
init(cell(5,3,2,b)).
init(cell(5,4,3,b)).
init(cell(5,5,2,b)).
init(cell(5,6,6,b)).
init(cell(6,1,2,b)).
init(cell(6,2,5,b)).
init(cell(6,3,4,b)).
init(cell(6,4,1,b)).
init(cell(6,5,3,b)).
init(cell(6,6,1,b)).
init(rol(o,0)).
init(control(o)).

%posibles valores que pueden tener las relaciones
base(control(X)):- role(X,_).
base(cell(X,Y,b)):- index(X),index(Y).
base(cell(X,Y,R)):- role(R,_),index(X),index(Y).

index(1).
index(2).
index(3).
index(4).
index(5).
index(6).

%posibles valores que pueden tener las entradas
input(R,mark(X,Y)):- role(R,_),index(X),index(Y).
input(R,noop):-role(R,_).

%movimientos legales
%Propios
legal(W,mark(X,Y)) :-
	t(\+cell(X,Y,_,W)),t(control(W)).

legal(o,mark(X,Y)) :-
	t(control(o)),
	(W is X + 2,t(cell(W,Y,_,b));
	 W is X + 2,t(cell(X,W,_,b));
	 W is X - 2,t(cell(W,Y,_,b));
	 W is X - 2,t(cell(X,W,_,b)):
	 W is X + 1,Z is Y + 1,t(cell(W,Z,_,b));
	 W is X - 1,Z is Y + 1,t(cell(W,Z,_,b));
	 W is X + 1,Z is Y - 1,t(cell(W,Z,_,b));
	 W is X - 1,Z is Y - 1,t(cell(W,Z,_,b))).

legal(x,mark(X,Y)) :-
	t(control(x)),
	(W is X + 1,t(cell(W,Y,_,_));
	 W is X + 1,t(cell(X,W,_,_));
	 W is X - 1,t(cell(W,Y,_,_));
	 W is X - 1,t(cell(X,W,_,_)):
	 W is X + 1,Z is Y + 1,t(cell(W,Z,_,_));
	 W is X - 1,Z is Y + 1,t(cell(W,Z,_,_));
	 W is X + 1,Z is Y - 1,t(cell(W,Z,_,_));
	 W is X - 1,Z is Y - 1,t(cell(W,Z,_,_)).


  % legal(o,mark(X,Y)) :-
	% 	W is X + 2,t(cell(W,Y,_,b)),t(control(o)).
  % legal(o,mark(X,Y)) :-
  % 	W is X + 2,t(cell(X,W,_,b)),t(control(o)).
  % legal(o,mark(X,Y)) :-
  % 	W is X - 2,t(cell(W,Y,_,b)),t(control(o)).
  % legal(o,mark(X,Y)) :-
  % 	W is X - 2,t(cell(X,W,_,b)),t(control(o)).
  % legal(o,mark(X,Y)) :-
  % 	W is X + 1,Z is Y + 1,t(cell(W,Z,_,b)),t(control(o)).
  % legal(o,mark(X,Y)) :-
  % 	W is X - 1,Z is Y + 1,t(cell(W,Z,_,b)),t(control(o)).
  % legal(o,mark(X,Y)) :-
  % 	W is X + 1,Z is Y - 1,t(cell(W,Z,_,b)),t(control(o)).
  % legal(o,mark(X,Y)) :-
  % 	W is X - 1,Z is Y - 1,t(cell(W,Z,_,b)),t(control(o)).




%Ta-te-ti
% legal(W,mark(X,Y)) :-
% 	t(cell(X,Y,b)),t(control(W)).
% legal(o,noop) :-
% 	t(control(x)).
% legal(x,noop) :-
%       t(control(o)).

%próximo estado
next(cell(M,N,x)) :-
      does(x,mark(M,N)),
      t(cell(M,N,b)).

next(cell(M,N,o)) :-
      does(o,mark(M,N)),
      t(cell(M,N,b)).

next(cell(M,N,W)) :-
      t(cell(M,N,W)),
      distinct(W,b).

next(cell(M,N,b)) :-
      does(_W,mark(J,_K)),
      t(cell(M,N,b)),
      distinct(M,J).

next(cell(M,N,b)) :-
      does(_W,mark(_J,K)),
      t(cell(M,N,b)),
      distinct(N,K).

next(control(o)) :-
      t(control(x)).

next(control(x)) :-
      t(control(o)).

%Cambiamos las reglas pierde el que hace TATETI
%goal(x,100) :- line(x),\+line(o).
%goal(x,50) :-  \+line(x), \+line(o).
%goal(x,0) :- \+line(x), line(o).

%goal(o,100) :- \+line(x), line(o).
%goal(o,50) :- \+line(x), \+line(o).
%goal(o,0) :- line(x), \+line(o).

goal(x,0) :- line(x),\+line(o).
%goal(x,50) :-  \+line(x), \+line(o).
goal(x,100) :- \+line(x), line(o).

goal(o,0) :- \+line(x), line(o).
%goal(o,50) :- \+line(x), \+line(o).
goal(o,100) :- role(o,6).


%Definicion de predicados auxiliares para puntaje de x
cantidad_materias(1,N).
cantidad_materias(1,N).
cantidad_materias(1,N).
cantidad_materias(1,N).
cantidad_materias(1,N).
cantidad_materias(1,N).
cantidad_materias(1,N).

line(Z) :- row(_M,Z).
    line(Z) :- column(_M,Z).
    line(Z) :- diagonal(Z).

    row(M,Z) :-
      t(cell(M,1,Z)) ,
      t(cell(M,2,Z)) ,
      t(cell(M,3,Z)).

    column(_M,Z) :-
      t(cell(1,N,Z)) ,
      t(cell(2,N,Z)) ,
      t(cell(3,N,Z)).

    diagonal(Z) :-
      t(cell(1,1,Z)) ,
      t(cell(2,2,Z)) ,
      t(cell(3,3,Z)).

    diagonal(Z) :-
      t(cell(1,3,Z)) ,
      t(cell(2,2,Z)) ,
      t(cell(3,1,Z)).

 terminal :- line(x).
 terminal :- line(o).
 terminal :- \+open.

 open :- t(cell(X,Y,b)),
 index(X),
 index(Y).



distinct(X,Y):-
X\==Y.

%inicializa estado inicial y borra historial
:-dynamic t/1,h/2,estado/1,does/2.
inicio:-
	retractall(t(_)),
	retractall(h(_,_)),
	retractall(estado(_)),
	crea_estado_inicial.

%crea estado inicial
crea_estado_inicial:-
	init(X),
    \+(t(X)),
	assert(t(X)),
    assert(h(0,X)),
	crea_estado_inicial.
crea_estado_inicial:-
	assert(estado(1)).


% gestor del juego
% borra acciones viejas
% busca nuevas acciones
% calcula próximo estado
% crea proximo estado

juego:- \+terminal,
 retractall(does(_X,_A)),
 inserta_acciones,
 proximo_estado,
 retractall(t(_Y)),
 crea_estado,
 imprime,
 juego.

juego:- terminal,
	goal(x,Px),goal(o,Po),
	display('x gano '),display(Px),display(' puntos y o gano '),display(Po),display(' puntos.').

% busca las nuevas acciones de los jugadores y las inserta
inserta_acciones:- t(control(X)),
   jugador(X,A), legal(X,A),
   assert(does(X,A)),
   role(O), distinct(X,O),
   assert(does(O,noop)).

%calcula el próximo estado
proximo_estado:-
        estado(E),
	next(Y),
	\+(h(E,Y)),
	assert(h(E,Y)),
	proximo_estado.
proximo_estado.

%crea el estado actual
crea_estado:-
	estado(E),
	h(E,Y),
	\+(t(Y)),
	assert(t(Y)),
	crea_estado.
crea_estado:-
 retract(estado(N)),
 N2 is N +1,
 assert(estado(N2)).

%imprime estado actual del juego
imprime:-
	estado(E),
	display('Estado: '),display(E),nl,
	t(control(X)),
	display('Control: '),display(X),nl,
	imprime_fila(1),
	imprime_fila(2),
	imprime_fila(3),
      imprime_fila(4),
      imprime_fila(5),
      imprime_fila(6),
	display('********').

imprime_fila(N):-
	t(cell(N,1,C1)),t(cell(N,2,C2)),t(cell(N,3,C3)),
      t(cell(N,4,C4)),t(cell(N,5,C5)),t(cell(N,6,C6)),
	display(C1),display(C2),display(C3),
      display(C4),display(C5),display(C6),nl.


%desarrollo jugador o
jugador(o,A):-
 legal(o,A).


%desarrollo jugador x
jugador(x,X):-
 %legal(x,X).
 display('Ingrese próximo movimiento:'),
 read(X).
% display('Ingrese Marca en Y:'),
% read_term(Y,[]).
%
/** <examples>
?- inicio,juego.
*/
%
