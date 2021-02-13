/* Autore: Aldo Franco Dragoni
 * Data: 13 febbraio 2021
 *
 * se due grafi diretti sono isomorfi il goal:
 * ?- iso(-ListaArchiE,-ListaArchiF,-ListaNodiE,-ListaNodiF,-Rel_archi)
 * ha successo e restituisce tutte le variabili argomento, ovvero:
 * ListaNodiE:  lista dei nodi del grafo 'e'
 * ListaNodiF:  lista dei nodi del grafo 'f'
 * ListaArchiE: lista degli archi del grafo 'e'
 * ListaArchiF: lista degli archi del grafo 'f'
 * Rel_archi: SOLUZIONE, ovvero la corrispondenza biunivoca fra gli archi dei 
 *            due grafi che ne determina l'isomorfismo
 */

% grafo diretto e
e(a,b).
e(b,c).
e(b,d).
e(a,c).
e(d,a).

% grafo diretto f
f(50,10).
f(11,50).
f(11,45).
f(10,45).
f(10,11).

iso(ListaArchiE,ListaArchiF,ListaNodiE,ListaNodiF,Rel_archi) :-
    findall((X,Y), e(X,Y), ListaArchiE),           % lista archi del grafo 'e'
    findall((X,Y), f(X,Y), ListaArchiF),           % lista archi del grafo 'f'
    setof(X, Y^(e(X,Y);e(Y,X)), ListaNodiE),        % lista nodi del grafo 'e'
    setof(X, Y^(f(X,Y);f(Y,X)), ListaNodiF),        % lista nodi del grafo 'f'
    !,
    rel_nodi(ListaNodiE,ListaNodiF,Rel_nodi), % relazioni fra nodi dei 2 grafi
    rel_archi(ListaArchiE,ListaArchiF,Rel_nodi,Rel_archi).         % SOLUZIONE

rel_nodi([],[],[]).
rel_nodi([Nodo_e|Resto_e], ListaNodiF, [(Nodo_e,Nodo_f)|R]) :-
    select(Nodo_f, ListaNodiF, Resto_f),
    rel_nodi(Resto_e, Resto_f, R).

rel_archi([],[],_,[]).
rel_archi([(Ep,Ed)|Resto_e],ListaArchiF,Rel_nodi,[(e(Ep,Ed),f(Fp,Fd))|R]) :-
    select((Fp,Fd),ListaArchiF,Resto_f),
    member((Ep,Fp),Rel_nodi),
    member((Ed,Fd),Rel_nodi),
    rel_archi(Resto_e,Resto_f,Rel_nodi,R).



