/* algoritmi per la RICERCA NELLO SPAZIO DEGLI STATI

Questo file contiene una raccolta di predicati per la ricerca nello spazio degli stati.
Lo spazio degli stati è descritto mediante un database di fatti
s(+StatoDoveMiTrovo,+StatoSuccessore,+EtichettaDellArco).
l'EtichettaDellArco non viene usata da tutti gli algoritmi.
Opzionalmente può essere presente un predicato
h(+Nodo,-Euristica)
che, o in forma di database di fatti ground, oppure in forma di regola, stabilisce il valore euristico del Nodo.
I vari algoritmi sono raccolti e gestiti da un lanciatore che ho predisposto allo scopo di uniformare il modo in cui possono essere mandati in esecuzione
cercaPercorso(+ETICHETTA,+NodoPartenza,-Percorso, +ProfonditàMassima)
dove ETICHETTA è una delle seguenti:
 dfs  : DFS
 dfsc : DFS con prevenzione cicli
 dfl  : DFS a profondità limitata
 dflsc: DFS a profondità limitata con prevenzione cicli
 ids  : Iterated Deepening Search
 ids2 : IDS con soluzioni multiple per risoddisfacimento
 bfs  : BFS
 bfsdl: BFS con "difference-lists"
 dfbf : DFS derivata da BFS
 bid  : Bidirezionale
 bidsc: Bidirezionale con prevenzione cicli nello spazio delle coppie
 bidcc: Bidirezionale con prevenzione cicli nello spazio degli stati
 ucs  : UCS
 gs   : Gready Search, serve l'euristica h(N,H)
 a    : A,             serve l'euristica h(N,H)
 best : A,             serve l'euristica h(N,H)
*/

cercaPercorso(dfs,  Nodo, Perc, _) :- dfs(Nodo,Perc). % depth first senza controllo sui cicli
cercaPercorso(dfsc, Nodo, Perc, _) :- dfsc([],Nodo,P), reverse(P,Perc). % depth first
cercaPercorso(dfl,  Nodo, Perc, P) :- dfl(Nodo,Perc,P).
cercaPercorso(dflsc,Nodo, Perc, P) :- dflsc([],Nodo,Crep,P), reverse(Crep,Perc).
cercaPercorso(ids,  Nodo, Perc, _) :- ids(Nodo,P,1), reverse(P,Perc).
cercaPercorso(ids2, Nodo, Perc, _) :- ids2(Nodo,P), reverse(P,Perc).
cercaPercorso(bfs,  Nodo, Perc, _) :- bfs([[Nodo]],P), reverse(P,Perc).
cercaPercorso(bfsdl,Nodo, Perc, _) :- bfsdl([[Nodo]|Z]-Z,Perc).
cercaPercorso(dfbf, Nodo, Perc, _) :- dfs_da_bfs([[Nodo]],Perc).
cercaPercorso(bid,  Nodo, Perc, _) :- destinazione(G), bid(bid,G,Nodo,Perc).
cercaPercorso(bidsc,Nodo, Perc, _) :- destinazione(G), bid(bidsc,G,Nodo,Perc).
cercaPercorso(bidcc,Nodo, Perc, _) :- destinazione(G), bid(bidcc,G,Nodo,Perc).
cercaPercorso(ucs,  Nodo, Perc, _) :- ucs([0-[Nodo]],C-P), reverse(P,Perc), writeln(C).
cercaPercorso(gs,   Nodo, Perc, _) :- gs([9999+0-[Nodo]],_+C-P), reverse(P,Perc), writeln(C).
cercaPercorso(a,    Nodo, Perc, _) :- a([9999+0-[Nodo]],_+C-P), reverse(P,Perc),writeln(C).
cercaPercorso(best, Nodo, Perc, _) :- best(Nodo,P),reverse(P,Perc).

% dfs(Partenza,Percorso): deep first search senza controllo cicli
dfs(N,[N]) :- destinazione(N). % sono arrivato a destinazione
dfs(DoveSto,[DoveSto|PercorsoDaSuccessoreaDestinazione]) :- %
	s(DoveSto,Successore,_),
	dfs(Successore,PercorsoDaSuccessoreaDestinazione).

% dfsc([],Partenza,Percorso): dfs con prevenzione dei cicli
dfsc(Perc,Nodo,[Nodo|Perc]) :- destinazione(Nodo).
dfsc(Perc,Nodo,Percorso) :-
	s(Nodo,Nodo1,_),
	\+  member(Nodo1,Perc),	% controllo di NON percorso ciclico
	dfsc([Nodo|Perc],Nodo1,Percorso).

% dfl(Nodo,Percorso,M): Percorso non più lungo di M, dal Nodo alla destinazione
dfl(Nodo,[Nodo],_)  :- destinazione(Nodo).
dfl(Nodo,[Nodo|Perc],Maxdepth)  :-
   Maxdepth > 0,
   s(Nodo,Nodo1,_),
   Max is Maxdepth - 1,
   dfl(Nodo1,Perc,Max).

% dflsc(Nodo,Percorso,M): deep first search senza cicli
dflsc(Perc,Nodo,[Nodo|Perc],_) :- destinazione(Nodo).
dflsc(Perc,Nodo,Percorso,Maxdepth) :-
	Maxdepth > 0,
	s(Nodo,Nodo1,_),
	\+  member(Nodo1,Perc),   % previene i cicli
	Max is Maxdepth - 1,
	dflsc([Nodo|Perc],Nodo1,Percorso,Max).

% ids(Nodo,Percorso,M): iterated deepening search
ids(Nodo,Percorso,Depth) :-
	dflsc([],Nodo,Percorso,Depth).
ids(Nodo,Percorso,Depth) :-
	Depth1 is Depth + 1,
	ids(Nodo,Percorso,Depth1).

% ids2(Nodo,Percorso,M) migliora rispetto ad ids perché non da
% soluzioni multiple per risoddisfacimento
ids2(Nodo, Percorso) :-
	ids2(Nodo,Goal,Percorso),
	destinazione(Goal).
ids2(N,N,[N]).
ids2(Primo,Ultimo,[Ultimo|P]) :-
	ids2(Primo,Penultimo,P),
	s(Penultimo,Ultimo,_),
	\+ member(Ultimo,P).

% bfs([Perc1,Perc2,...],Percorso): breadth first search
% Percorso è estensione di uno dei Perc e raggiunge la destinazione
bfs([[Nodo|Percorso]|_],[Nodo|Percorso]) :- destinazione(Nodo).
bfs([Percorso|Percorsi],Soluzione):-
  espansione(Percorso,PercorsiEstesi),
  append(Percorsi,PercorsiEstesi,NuoviPercorsi),
  bfs(NuoviPercorsi,Soluzione).

espansione([N|Parziale],Percorsi):-
  findall([NN,N|Parziale],(s(N,NN,_),\+ member(NN,Parziale)),Percorsi).

% bfsdl(([[Partenza]|Z]-Z,Percorso)
% ricerca in ampiezza con "difference-lists"
bfsdl([[Nodo|Perc]|_]-_,[Nodo|Perc]) :- destinazione(Nodo).
bfsdl([Percorso|Percorsi]-Z,Soluzione) :-
	espansione(Percorso,PercorsiEstesi),
	append(PercorsiEstesi,Z1,Z),
	Percorsi \== Z1,
	bfsdl(Percorsi-Z1,Soluzione).

% dfs_da_bfs([[Partenza]],Percorso)
% ricerca in profondità derivata da quella in ampiezza
% inserendo i nuovi percorsi frontiera in testa invece che in coda
dfs_da_bfs([[Nodo|Perc]|_],[Nodo|Perc]) :- destinazione(Nodo).
dfs_da_bfs([Percorso|Percorsi],Soluzione):-
  espansione(Percorso,PercorsiEstesi),
  append(PercorsiEstesi,Percorsi,NuoviPercorsi),
  dfs_da_bfs(NuoviPercorsi,Soluzione).

% bid(FLAG, Goal, Partenza, Percorso):
% implementazioni della ricerca bidirezionale
bid(bid, G, Nodo,Perc) :- % in profondità senza controllo cicli
	biddfs1(Nodo-G,PercorsoCoppie), % percorso spazio coppie
	estrae(PercorsoCoppie,Perc). % percorso spazio normale
bid(bidsc, G, Nodo,Perc) :- % profondità con controllo cicli coppie
	biddfs2([],Nodo-G,CoppiePercorso),
	reverse(CoppiePercorso,PercorsoCoppie),
	estrae(PercorsoCoppie,Perc).
bid(bidcc, G, Nodo,Perc) :- % profondità con controllo cicli stati
	biddfs3([],Nodo-G,CoppiePercorso),
	reverse(CoppiePercorso,PercorsoCoppie),
	estrae(PercorsoCoppie,Perc).

biddfs1(N,[N]) :- nuova_destinazione(N).
biddfs1(N,[N|C]) :-
	nuovo_successore(N,N1),
	biddfs1(N1,C).

biddfs2(Perc,N,[N|Perc]) :- nuova_destinazione(N).
biddfs2(Perc,N,Perc) :-
	nuovo_successore(N,S-E),
	\+ member(S-E, Perc),  % previene cicli sulla coppia
	biddfs2([N|Perc],S-E,Perc).

biddfs3(Perc,N,[N|Perc]) :- nuova_destinazione(N).
biddfs3(Perc,N,Perc) :-
	nuovo_successore(N,S-E),
	\+ member(S-_, Perc),  % previene cicli sullo stato
	\+ member(_-S, Perc),  % previene cicli sullo stato
	\+ member(E-_, Perc),  % previene cicli sullo stato
	\+ member(_-E, Perc),  % previene cicli sullo stato
	biddfs3([N|Perc],S-E,Perc).

estrae(PercorsoCoppie,Perc) :-
	estrae(PercorsoCoppie,Prima,Invertita),
	reverse(Invertita,Seconda),
	append(Prima,Seconda,Perc).
estrae([],[],[]).
estrae([S-E|C],[S|Prima],[E|Invertita]) :-
       estrae(C,Prima,Invertita).

nuovo_successore(S-E,S1-E1):-
   s(S,S1,_),      % un passo in avanti da Start
   s(E1,E,_).      % un passo indietro da End

nuova_destinazione(S-S).             % i due goals coincidono
nuova_destinazione(S-E) :- s(S,E,_). % i due goals sono adiacenti

% ucs([0-[Partenza]],C-Percorso)
% ricerca a costo uniforme derivata da quella in ampiezza
% il C-[Percorso] ha un costo C dato dalla somma dei costi degli archi

ucs([C-[Nodo|Perc]|_],C-[Nodo|Perc]) :- destinazione(Nodo).
ucs([C-Percorso|Percorsi],Soluzione):-
  espansione2(C-Percorso,PercorsiEstesi),
  mergeSort(PercorsiEstesi,Percorsi,NuoviPercorsi),
  ucs(NuoviPercorsi,Soluzione).

espansione2(C-[N|P],Percorsi):-
  setof(CC-[NN,N|P],C1^(s(N,NN,C1),\+ member(NN,P),CC is C+C1),Percorsi),!.
espansione2(_-_,[]). % non fa fallire espansione2

mergeSort([],L,L).
mergeSort(L,[],L).
mergeSort([C1-P1|CP1],[C2-P2|CP2],[C1-P1|Coda]) :-
	C1 =< C2,
	mergeSort(CP1,[C2-P2|CP2],Coda).
mergeSort([C1-P1|CP1],[C2-P2|CP2],[C2-P2|Coda]) :-
	C1 > C2,
	mergeSort([C1-P1|CP1],CP2,Coda).

% gs([H+C-[Partenza]],H+C-Percorso)
% ricerca greedy derivata da quella a costo uniforme
% il H+C-[Percorso] ha un costo C dato dalla somma dei costi degli archi
% e l'euristica del nodo frontiera è H
gs([0+C-[Nodo|Perc]|_],0+C-[Nodo|Perc]). % h=0 siamo al GOAL
gs([H+C-Percorso|Percorsi],Soluzione):-
  espansione_gs(H+C-Percorso,PercorsiEstesi),
  mergeSort_h(PercorsiEstesi,Percorsi,NuoviPercorsi),
  gs(NuoviPercorsi,Soluzione).

espansione_gs(_+C-[N|P],Percorsi):- % espansione gready
  setof(HH+CC-[NN,N|P],C1^(s(N,NN,C1),\+ member(NN,P),h(NN,HH),CC is C+C1),Percorsi),
  !.
espansione_gs(_+_-_,[]). % non fa fallire espansione_gs

% fusione con euristica
mergeSort_h([],L,L).
mergeSort_h(L,[],L).
mergeSort_h([H1+C1-P1|CP1],[H2+C2-P2|CP2],[H1+C1-P1|Coda]) :-
	H1 =< H2,
	mergeSort_h(CP1,[H2+C2-P2|CP2],Coda).
mergeSort_h([H1+C1-P1|CP1],[H2+C2-P2|CP2],[H2+C2-P2|Coda]) :-
	H1 > H2,
	mergeSort_h([H1+C1-P1|CP1],CP2,Coda).

% a([F+0-[Partenza]],F+C-Percorso)
% ricerca A derivata da quella in ampiezza
% il F+C-[Percorso] ha una valore F dato dalla somma dei costo del
% nodo CC più la sua euristica H
a([C+C-[Nodo|Perc]|_],C+C-[Nodo|Perc]). % F=C siamo al GOAL
a([F+C-Percorso|Percorsi],Soluzione):-
  espansione_a(F+C-Percorso,PercorsiEstesi),
  mergeSort_h(PercorsiEstesi,Percorsi,NuoviPercorsi),
  a(NuoviPercorsi,Soluzione).

espansione_a(_+C-[N|P],Percorsi):-
  setof(FF+CC-[NN,N|P],C1^H^(s(N,NN,C1),\+ member(NN,P),h(NN,H),CC is C+C1,FF is CC+ H),Percorsi),
  !.
espansione_a(_+_-_,[]). % non fa fallire espansione_a

/*
% ucs([0-[Partenza]],C-Percorso)
% restituisce solo le prime due migliori soluzioni ma tiene meno
% percorsi in memoria perché elimina quelli che riespandono un nodo già
% espanso
% bisogna lanciare
% retractall(o(_,_)).
% ogni volta prima di utilizzarlo

ucs([C-[Nodo|Perc]|_],C-[Nodo|Perc]) :- destinazione(Nodo).
ucs([C-[N|P]|Percorsi],Soluzione):-
	ottimo(N,C),
	espansione3(C-[N|P],PercorsiEstesi),
	mergeSort(PercorsiEstesi,Percorsi,NuoviPercorsi),
	ucs(NuoviPercorsi,Soluzione).

espansione3(C-[N|P],Percorsi):-
	setof(CC-[NN,N|P],C1^(s(N,NN,C1),\+ o(NN,_),CC is C+C1),Percorsi),!.
espansione3(_-_,[]). % non fa fallire espansione2

% restituisce il costo ottimo se il nodo è stato già espanso
% altrimenti lo asserisce
ottimo(N,C) :-
	o(N,C),
	!.
ottimo(N,C) :-
	assertz(o(N,C)).
*/

% altra implementazione di A
% si parte espandendo un albero che ha solo una foglia e costo 0
% assumiamo 9999 maggiore di qualunque valore di F
best(Nodo, Percorso) :-
  espande([], l(Nodo, 0/0), 9999, _, si, Percorso).

% espande(+Perc, +Alb, +Lim, -Alb1, -Risolto, -Percorso):
% Perc:  percorso fra il nodo radice ed il sottoalbero Alb
% Alb: sottoalbero corrente
% Lim: valore di F massimo entro cui espandere Alb
% Alb1: Alb espanso fino al limite Lim; conseguentemente il
%   valore di F di Alb1 è maggiore di quello di Alb, a meno che
%   non si è trovato il goal
% Risolto: [sì,no,mai] indica se si è trovato un nodo goal
%   nell'espansione di Alb
% Percorso: un percorso fra il nodo radice ed il goal passando per
%   Alb1 all'interno del limite Lim

%  1: Risolto=si
%  se si trova il goal si aggiunge il sottoalbero (che è solo una
%  foglia) al Percorso e lo si restituisce in Percorso
espande(Percorso,l(N,F/_),_,_,si,[N|Percorso]) :- destinazione(N), writeln(F).

%  2: nodo-foglia, valore di f minore di Lim
%  genera i successori e li espande fino a Lim
espande(P, l(N,F/G), Lim, Alb1, Risolto, Sol)  :-
  F  =<  Lim,
  (  bagof(Suc/C,(s(N,Suc,C),\+ member(Suc,P)),Successori), % successori
     !,				      % il nodo N ha dei successori
     listsucc(G, Successori, Sottoalberi),  % crea la lista dei sottoalberi
     bestf(Sottoalberi, F1),	      % valore di F del miglior sottoalbero
     espande(P, t(N,F1/G,Sottoalberi), Lim, Alb1, Risolto, Sol)
     ;
     Risolto = mai    % il nodo N non ha successori - ramo morto
  ) .

%  3: nodo-NON-foglia, F < Lim
%  espande il sottoalbero più promettente; a seconda del
%  risultato il predicato continua decide come procedere
espande(P, t(N,F/G,[T|Ts]), Lim, Alb1, Risolto, Sol)  :-
  F =<	Lim,
  bestf(Ts,BF),
  Lim1 is min(Lim,BF),
  espande([N|P], T, Lim1, T1, Risolto1, Sol),
  continua(P, t(N,F/G,[T1|Ts]), Lim, Alb1, Risolto1, Risolto, Sol).

%  4: nodo-NON-foglia che non ha più sottoalberi da processare
%  è un ramo morto che non sarà mai risolto
espande( _, t(_,_,[]), _, _, mai, _) :- !.

%  5: Risolto=no, F > Lim
%  Alb non cresce
espande(_, Alb, Lim, Alb, no, _)  :-
  f(Alb, F), F > Lim.

% continue(Perc, Alb, Lim, NAlb, SottoAlbRisolto, AlbRisolto, Percorso)
continua(_, _, _, _, si, si, _).
continua(P, t(N,_/G,[T1|Ts]), Lim, Alb1, no, Risolto, Sol)  :-
  inserisce(T1, Ts, NTs),
  bestf(NTs, F1),
  espande(P, t(N,F1/G,NTs), Lim, Alb1, Risolto, Sol).
continua(P, t(N,_/G,[_|Ts]), Lim, Alb1, mai, Risolto, Sol)  :-
  bestf(Ts, F1),
  espande(P, t(N,F1/G,Ts), Lim, Alb1, Risolto, Sol).

% listsucc(G0, [Nodo1/Cost1, ...], [l(MigliorNodo,BestF/G), ...]):
% produce la lista delle foglie ordinate in base ad F
listsucc(_, [], []).
listsucc(G0, [N/C|NCoda], Ts)  :-
  G is G0 + C,
  h(N, H),	     % prende l'euristica h(N)
  F is G + H,
  listsucc(G0, NCoda, Ts1),
  inserisce(l(N,F/G), Ts1, Ts).

% Inserisce T nella lista degli alberi Ts preservando l'ordinamento F
inserisce(T,Ts,[T|Ts])  :-
  f(T,F),
  bestf(Ts,F1),
  F  =<  F1, !.
inserisce(T, [T1|Ts], [T1|Ts1]) :-
  inserisce(T,Ts,Ts1).

% estrae il valore di F
f(l(_,F/_), F).        % da una foglia
f(t(_,F/_,_), F).      % da un albero

bestf([T|_], F)	:- f(T, F).  % valore di F del miglior albero
bestf([], 9999).       % nessun albero, F massimo
