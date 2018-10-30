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
		% t(cell(W,Y,_,o)),W is X + 2,t(cell(X,Y,_,b)),t(control(o)).
    t(cell(W,Y,_,o)),X is W - 2,t(cell(X,Y,_,b)),t(control(o)).
  legal(o,move(X,Y)) :-
  	% t(cell(X,W,_,o)),W is Y + 2,t(cell(X,Y,_,b)),t(control(o)).
    t(cell(X,W,_,o)),Y is W - 2,t(cell(X,Y,_,b)),t(control(o)).
  legal(o,move(X,Y)) :-
  	% t(cell(W,Y,_,o)),W is X - 2,t(cell(X,Y,_,b)),t(control(o)).
    t(cell(W,Y,_,o)),X is W + 2,t(cell(X,Y,_,b)),t(control(o)).
  legal(o,move(X,Y)) :-
  	% t(cell(X,W,_,o)),W is Y - 2,t(cell(X,Y,_,b)),t(control(o)).
    t(cell(X,W,_,o)),Y is W + 2,t(cell(X,Y,_,b)),t(control(o)).
  legal(o,move(X,Y)) :-
  	% t(cell(W,Z,_,o)),W is X + 1,Z is Y + 1,t(cell(X,Y,_,b)),t(control(o)).
    t(cell(W,Z,_,o)),X is W - 1,Y is Z - 1,t(cell(X,Y,_,b)),t(control(o)).
  legal(o,move(X,Y)) :-
  	% t(cell(W,Z,_,o)),W is X - 1,Z is Y + 1,t(cell(X,Y,_,b)),t(control(o)).
    t(cell(W,Z,_,o)),X is W + 1,Y is Z - 1,t(cell(X,Y,_,b)),t(control(o)).
  legal(o,move(X,Y)) :-
  	% t(cell(W,Z,_,o)),W is X + 1,Z is Y - 1,t(cell(X,Y,_,b)),t(control(o)).
    t(cell(W,Z,_,o)),X is W - 1,Y is Z + 1,t(cell(X,Y,_,b)),t(control(o)).
  legal(o,move(X,Y)) :-
  	% t(cell(W,Z,_,o)),W is X - 1,Z is Y - 1,t(cell(X,Y,_,b)),t(control(o)).
    t(cell(W,Z,_,o)),X is W + 1,Y is Z + 1,t(cell(X,Y,_,b)),t(control(o)).

	legal(x,move(X,Y)) :-
		%t(cell(W,Y,_,x)),W is X + 1,t(cell(X,Y,_,b)),t(control(x)).
    t(cell(W,Y,_,x)),X is W - 1,t(cell(X,Y,_,b)),t(control(x)).
  legal(x,move(X,Y)) :-
		%t(cell(X,W,_,x)),W is Y + 1,t(cell(X,Y,_,b)),t(control(x)).
    t(cell(X,W,_,x)),Y is W - 1,t(cell(X,Y,_,b)),t(control(x)).
  legal(x,move(X,Y)) :-
		%t(cell(W,Y,_,x)),W is X - 1,t(cell(X,Y,_,b)),t(control(x)).
    t(cell(W,Y,_,x)),X is W + 1,t(cell(X,Y,_,b)),t(control(x)).
  legal(x,move(X,Y)) :-
		%t(cell(X,W,_,x)),W is Y - 1,t(cell(X,Y,_,b)),t(control(x)).
    t(cell(X,W,_,x)),Y is W + 1,t(cell(X,Y,_,b)),t(control(x)).
	legal(x,move(X,Y)) :-
		%t(cell(W,Z,_,x)),W is X + 1,Z is Y + 1,t(cell(X,Y,_,b)),t(control(x)).
    t(cell(W,Z,_,x)),X is W - 1,Z is Y + 1,t(cell(X,Y,_,b)),t(control(x)).
	legal(x,move(X,Y)) :-
		%t(cell(W,Z,_,x)),W is X - 1,Z is Y + 1,t(cell(X,Y,_,b)),t(control(x)).
    t(cell(W,Z,_,x)),X is W + 1,Z is Y + 1,t(cell(X,Y,_,b)),t(control(x)).
	legal(x,move(X,Y)) :-
		%t(cell(W,Z,_,x)),W is X + 1,Z is Y - 1,t(cell(X,Y,_,b)),t(control(x)).
    t(cell(W,Z,_,x)),X is W - 1,Z is Y - 1,t(cell(X,Y,_,b)),t(control(x)).
	legal(x,move(X,Y)) :-
		%t(cell(W,Z,_,x)),W is X - 1,Z is Y - 1,t(cell(X,Y,_,b)),t(control(x)).
    t(cell(W,Z,_,x)),X is W + 1,Z is Y - 1,t(cell(X,Y,_,b)),t(control(x)).
%próximo estado
next(cell(M,N,0,x)) :-
      does(x,move(M,N)),
			t(cell(M,N,Num,b)).

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


% Predicado auxiliar para encontrar que materia tiene menos apariciones
materia_x(Min):-
  t(materia(GO)), %Materia actual
  % findall(X,between(1,GO,X),MateriasYaCursadas), %Todas las materias 'cursadas'
  % subtract([1,2,3,4,5,6],MateriasYaCursadas,Materias), %Removemos las 'cursadas'
  % cantidades(Materias,R), %Buscamos las cantidades de las no cursadas
  % min_list(R,Max). %Cual es el mayor numero
  Ma is GO +1,
  sinlistas(Ma,7,7,Min).

% Mira mama! Sin listas
sinlistas(6,_CantA,Min,Min).

sinlistas(MA,CantA,MinA,Min):-
  t(cantidad(MA,Cant)),
  N is MA + 1,
  ((Cant < CantA,
  sinlistas(N,Cant,Cant,Min));
  sinlistas(N,CantA,MinA,Min)).

% Buscamos la cantidad que le falta a cada materia de la lista
% cantidades([0|T],R):-
%   cantidades(T,R).
% cantidades([H|T],[C|R]):-
%   t(cantidad(H,C)),
%   cantidades(T,R).
% cantidades([],[]).



goal(x,M) :- materia_x(N),M is (6-N)*100/6.
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
 legal(o,A).
 % ansi_format([bold,fg(green)], '~w', ['JUGADOR O: Ingrese próximo movimiento: ']),
 % read(A).
 % greedy(o,A),nl.

%desarrollo jugador x
jugador(x,X):-
 %legal(x,X).
 ansi_format([bold,fg(green)], '~w', ['JUGADOR X: Ingrese próximo movimiento: ']),
 greedy(x,X),nl.
% read(X).
% display('Ingrese Marca en Y:'),
% read_term(Y,[]).
%


/** <examples>
?- inicio,juego.
*/
%

% Desarrollo del Agente

  % Encontrar todos los estados posibles y legales
  % get_estados_legales(+Rol,-Legales)
	get_estados_legales(Rol,Legales):-
		findall(X,input(Rol,X),Posibles),include(legal(Rol),Posibles,Legales).

  % Simular movimiento A para el rol J
  % Simular_movimiento(+Rol,+Movimiento,-ListadoT,-Estado,-Goal)
  simular_movimiento(J,A,ListaT,N,Goal):-
    %Guardamos el estado del juego antes de modificarlo
    findall(X,t(X),ListaT),
    estado(N),
    %Simulamos que elegimos el movimiento
    retractall(does(_X,_A)),
    assert(does(J,A)),
    role(O), distinct(J,O),
    assert(does(O,noop)),
    %Continuamos con el juego
    proximo_estado,
    %h(E,Y),
    retractall(t(_Y)),
    crea_estado,
     % imprime,
    % Verificamos el goal del estado
    goal(J,Goal).%,display(Goal),nl,
    % Retractamos el movimiento
    % N2 is N + 1,
    % retractall(does(_X,_A)),
    % retractall(t(_)),
    % retractall(h(N2,_)),
    % retract(estado(_)),
    % maplist(crear_t,ListaT),
    % assert(estado(N)),
    % % assert(t(E)),
    % imprime.


  % deshacer_movimiento(+ListaT,+Estado)
  % ListaT contiene todos los True hasta antes del estado simulado
  % Estado es el numero que le corresponde al Estado anterior a la simulacion
  deshacer_movimiento(ListaT,N):-
    % Retractamos el movimiento
     N2 is N + 1,
    retractall(does(_X,_A)),
    retractall(t(_A)),
    retractall(h(N,_B)),
    retract(estado(N2)),
    maplist(crear_t,ListaT),
    assert(estado(N)).
    %assert(t(E)),
    % imprime.

  % Predicado auxiliar para crear aserciones t
  crear_t(X):-
    assert(t(X)).

  % Dado un movimiento, calcula la ganancia
  ganancia(Rol,Movimiento,Ganancia):-
    simular_movimiento(Rol,Movimiento,ListaT,Estado,Ganancia),
    deshacer_movimiento(ListaT,Estado).

  greedy(Rol,MejorJugada):-
    get_estados_legales(Rol,[HL|TL]),
    % display([HL|TL]),nl,
    ganancia(Rol,HL,G),
    greedy1(Rol,TL,G,HL,MejorJugada),
    display(MejorJugada).


  greedy1(_Rol,[],_MejorGA,MejorA,MejorA).

  greedy1(Rol,[H|T],MejorGA,MejorA,Mejor):-
  %  display(T),
    ganancia(Rol,H,G),
    % display(H),display(': '),display(G),nl,
    ((G > MejorGA,greedy1(Rol,T,G,H,Mejor));
    (greedy1(Rol,T,MejorGA,MejorA,Mejor))).

  % greedy1(Rol,[_H|T],MejorGA,MejorA,Mejor):-
  %   greedy1(Rol,T,MejorGA,MejorA,Mejor).



  ejemplo(R,M,GA,G):-
    legal(R,M),
    fail.

  ejemplo(R,M,GA,G):- !.
