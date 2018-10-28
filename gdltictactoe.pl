%para ejecutar ?-inicio,juego. (Run!)

%roles o y x
role(o).
role(x).

%estado inicial:
%cell(X,Y,NumeroMateria,OcupadoPor)
%X coordenada x.
%Y coordenada y.
%NumeroMateria representa una materia de 1 a 6.
%OcupadoPor: jugador x, jugador o, o blanco.
%listas de materias
% init(lista(1,[[1,2],[2,6],[4,1],[4,3],[6,4],[6,6]])).
% init(lista(2,[[1,3],[2,5],[4,6],[5,3],[5,5],[6,1]])).
% init(lista(3,[[1,4],[2,3],[3,2],[5,1],[5,4],[6,5]])).
% init(lista(4,[[2,1],[3,4],[3,6],[4,5],[5,2],[6,3]])).
% init(lista(5,[[1,6],[2,2],[2,4],[3,1],[3,5],[6,2]])).
% init(lista(6,[[1,1],[1,5],[3,3],[4,2],[4,4],[5,6]])).
init(cantidad(1,6)).
init(cantidad(2,6)).
init(cantidad(3,6)).
init(cantidad(4,6)).
init(cantidad(5,6)).
init(cantidad(6,6)).
init(materia(0)).
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
init(cell(2,5,2,x)).
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
init(cell(6,1,2,o)).
init(cell(6,2,5,b)).
init(cell(6,3,4,b)).
init(cell(6,4,1,b)).
init(cell(6,5,3,b)).
init(cell(6,6,1,b)).
init(rol(o,0)).
init(control(o)).

%posibles valores que pueden tener las relaciones
base(control(X)):- role(X).
base(cell(X,Y,_,b)):- index(X),index(Y).
base(cell(X,Y,_,R)):- role(R),index(X),index(Y).
base(lista(Numero,_LL)):- index(Numero).

index(1).
index(2).
index(3).
index(4).
index(5).
index(6).

%posibles valores que pueden tener las entradas
input(R,move(X,Y)):- role(R),index(X),index(Y).
input(R,noop):-role(R).

%movimientos legales
%Propios
% legal(W,move(X,Y)) :-
% 	t(\+cell(X,Y,_,W)),t(control(W)).

% legal(o,move(X,Y)) :-
% 	t(control(o)),
% 	((W is X + 2,t(cell(W,Y,_,b)));
% 	 (W is Y + 2,t(cell(X,W,_,b)));
% 	 (W is X - 2,t(cell(W,Y,_,b)));
% 	 (W is Y - 2,t(cell(X,W,_,b)));
% 	 (W is X + 1,Z is Y + 1,t(cell(W,Z,_,b)));
% 	 (W is X - 1,Z is Y + 1,t(cell(W,Z,_,b)));
% 	 (W is X + 1,Z is Y - 1,t(cell(W,Z,_,b)));
% 	 (W is X - 1,Z is Y - 1,t(cell(W,Z,_,b)))).
%
% legal(x,move(X,Y)) :-
% 	t(control(x)),
% 	((W is X + 1,t(cell(W,Y,_,b)));
% 	 (W is Y + 1,t(cell(X,W,_,b)));
% 	 (W is X - 1,t(cell(W,Y,_,b)));
% 	 (W is Y - 1,t(cell(X,W,_,b)));
% 	 (W is X + 1,Z is Y + 1,t(cell(W,Z,_,b)));
% 	 (W is X - 1,Z is Y + 1,t(cell(W,Z,_,b)));
% 	 (W is X + 1,Z is Y - 1,t(cell(W,Z,_,b)));
% 	 (W is X - 1,Z is Y - 1,t(cell(W,Z,_,b)))).

legal(o,noop) :-
 	t(control(x)).
legal(x,noop) :-
   t(control(o)).

  legal(o,move(X,Y)) :-
		t(cell(W,Y,_,o)),W is X + 2,t(cell(X,Y,_,b)),t(control(o)).
  legal(o,move(X,Y)) :-
  	t(cell(X,W,_,o)),W is Y + 2,t(cell(X,Y,_,b)),t(control(o)).
  legal(o,move(X,Y)) :-
  	t(cell(W,Y,_,o)),W is X - 2,t(cell(X,Y,_,b)),t(control(o)).
  legal(o,move(X,Y)) :-
  	t(cell(X,W,_,o)),W is Y - 2,t(cell(X,Y,_,b)),t(control(o)).
  legal(o,move(X,Y)) :-
  	t(cell(W,Z,_,o)),W is X + 1,Z is Y + 1,t(cell(X,Y,_,b)),t(control(o)).
  legal(o,move(X,Y)) :-
  	t(cell(W,Z,_,o)),W is X - 1,Z is Y + 1,t(cell(X,Y,_,b)),t(control(o)).
  legal(o,move(X,Y)) :-
  	t(cell(W,Z,_,o)),W is X + 1,Z is Y - 1,t(cell(X,Y,_,b)),t(control(o)).
  legal(o,move(X,Y)) :-
  	t(cell(W,Z,_,o)),W is X - 1,Z is Y - 1,t(cell(X,Y,_,b)),t(control(o)).

	legal(x,move(X,Y)) :-
		t(cell(W,Y,_,x)),W is X + 1,t(cell(X,Y,_,b)),t(control(x)).
	legal(x,move(X,Y)) :-
		t(cell(X,W,_,x)),W is Y + 1,t(cell(X,Y,_,b)),t(control(x)).
	legal(x,move(X,Y)) :-
		t(cell(W,Y,_,x)),W is X - 1,t(cell(X,Y,_,b)),t(control(x)).
	legal(x,move(X,Y)) :-
		t(cell(X,W,_,x)),W is Y - 1,t(cell(X,Y,_,b)),t(control(x)).
	legal(x,move(X,Y)) :-
		t(cell(W,Z,_,x)),W is X + 1,Z is Y + 1,t(cell(X,Y,_,b)),t(control(x)).
	legal(x,move(X,Y)) :-
		t(cell(W,Z,_,x)),W is X - 1,Z is Y + 1,t(cell(X,Y,_,b)),t(control(x)).
	legal(x,move(X,Y)) :-
		t(cell(W,Z,_,x)),W is X + 1,Z is Y - 1,t(cell(X,Y,_,b)),t(control(x)).
	legal(x,move(X,Y)) :-
		t(cell(W,Z,_,x)),W is X - 1,Z is Y - 1,t(cell(X,Y,_,b)),t(control(x)).

%próximo estado
next(cell(M,N,0,x)) :-
      does(x,move(M,N)),
			t(cell(M,N,_Num,b)).

next(cell(M,N,0,x)) :-
      does(x,noop),
			t(cell(M,N,_Num,x)).

next(cell(M,N,Num,o)) :-
      does(o,noop),
			t(cell(M,N,Num,o)).

next(cell(M,N,Num,o)) :-
      does(o,move(M,N)),
      t(cell(M,N,Num,b)).

% next(cell(M,N,Num,b)) :-
% 			%does(x,move(M,N)),
%       t(cell(M,N,Num,x)).

next(cell(M,N,Num,b)) :-
			does(W,move(X,_Y)),
			distinct(M,X),
      t(cell(M,N,Num,W)).

next(cell(M,N,Num,b)) :-
			does(W,move(_X,Y)),
			distinct(Y,N),
      t(cell(M,N,Num,W)).

% next(cell(M,N,Num,b)) :-
      % t(cell(M,N,Num,b)).

next(cell(M,N,Num,b)) :-
      does(_W,move(J,_K)),
      t(cell(M,N,Num,b)),
      distinct(M,J).

next(cell(M,N,Num,b)) :-
      does(_W,move(_J,K)),
      t(cell(M,N,Num,b)),
      distinct(N,K).

next(control(o)) :-
      t(control(x)).

next(control(x)) :-
      t(control(o)).

next(cantidad(Mat,NN)):-
			does(x,move(M,N)),
			t(cell(M,N,Mat,_B)),
			t(cantidad(Mat,Otra)),
			NN is Otra-1.

next(cantidad(Mat,NN)):-
			does(x,move(M,N)),
			t(cell(M,N,Mat2,_B)),
			distinct(Mat,Mat2),
			t(cantidad(Mat,NN)).

next(cantidad(Mat,NN)):-
			does(x,noop),
			t(cantidad(Mat,NN)).

next(materia(NNum)):-
			does(o,move(M,N)),
			t(cell(M,N,NNum,_B)),
			t(materia(Num)),
			succ(Num,NNum).

next(materia(Num)):-
		does(o,move(M,N)),
		t(cell(M,N,NNum,_B)),
		t(materia(Num)),
		\+succ(Num,NNum).

next(materia(Mat)):-
		does(o,noop),
		t(materia(Mat)).

%Predicado auxiliar para encontrar la lista de menor long.
% select_element(Goal, [Head | Tail], Selected) :-
%     select_element(Goal, Tail, Head, Selected).
%
%
% select_element(_Goal, [], Selected, Selected).
%
% select_element(Goal, [Head | Tail], Current, FinalSelected) :-
%     call(Goal, Head, Current, Selected),
%     select_element(Goal, Tail, Selected, FinalSelected).
%
% get_bigger_number(N1, N2, N) :-
%     N is max(N1, N2).
%
% get_bigger_list(L1,L2,L2) :-
% 		length(L1,N1),
% 		length(L2,N2),
% 		N1>=N2.
%
% get_bigger_list(L1,L2,L1) :-
% 		length(L1,N1),
% 		length(L2,N2),
% 		N1<N2.
%
% listas(N,[LL1|LL]):-
% 	t(lista([M,LL1])),
% 	\+ t(lista([M,LL1])),
% 	N<M,
% 	listas(N,LL).
%
% listas(_N,[]).

%Cambiamos las reglas pierde el que hace TATETI
goal(x,M) :- t(materia(N)),M is (6-N)*100/6.
goal(o,M) :- t(materia(X)),M is X * 100/6.


 terminal :- \+open.
 open :- t(materia(N)),succ(N,Su),t(cantidad(Su,X)),X>0.



distinct(X,Y):-
X\==Y.

% distinct2([X,Y],[A,B]):-
% 	X\==A,
% 	Y\==B.

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
 imprime.
 %juego.

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
	imprime_regla,
	imprime_fila(2),
	imprime_regla,
	imprime_fila(3),
	imprime_regla,
  imprime_fila(4),
	imprime_regla,
  imprime_fila(5),
	imprime_regla,
  imprime_fila(6),
	imprime_regla,
	display('********'),nl,
	display('Cursando la Materia: '),
	t(materia(Mat)),display(Mat),nl.

imprime_fila(N):-
	t(cell(N,1,C1,OC1)),t(cell(N,2,C2,OC2)),t(cell(N,3,C3,OC3)),
      t(cell(N,4,C4,OC4)),t(cell(N,5,C5,OC5)),t(cell(N,6,C6,OC6)),
	display(C1),imprime_color(OC1),display('|'),display(C2),imprime_color(OC2),display('|'),
	display(C3),imprime_color(OC3),display('|'),display(C4),imprime_color(OC4),display('|'),
	display(C5),imprime_color(OC5),display('|'),display(C6),imprime_color(OC6),display('|'),nl.
imprime_regla:-
	display('-'),display('-'),display('-'),display('-'),display('-'),display('-'),nl.

	imprime_lista(N):-
		t(lista(N,Ll)),display(Ll),nl.

imprime_color(b):-
		display(b).

imprime_color(o):-
		ansi_format([bold,fg(blue)], '~w', [o]).

imprime_color(x):-
		ansi_format([bold,fg(red)], '~w', [x]).

imprime_color(Char):-
	display(Char).

%desarrollo jugador o
jugador(o,A):-
 %legal(o,A).
 ansi_format([bold,fg(green)], '~w', ['JUGADOR O: Ingrese próximo movimiento: ']),
 read(A).

%desarrollo jugador x
jugador(x,X):-
 %legal(x,X).
 ansi_format([bold,fg(green)], '~w', ['JUGADOR X: Ingrese próximo movimiento: ']),
 read(X).
% display('Ingrese Marca en Y:'),
% read_term(Y,[]).
%


/** <examples>
?- inicio,juego.
*/
%
