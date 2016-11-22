%Variables
bourse([[ble,7],[riz,6],[cacao,6],[cafe,6],[sucre,6],[mais,6]]).
piles([
	[mais, riz, ble, ble],
	[ble, mais, sucre, riz],
	[cafe, sucre, cacao, riz],
	[cafe, mais, sucre, mais],
	[cacao, mais, ble, sucre],
	[riz, cafe, sucre, ble],
	[cafe, ble, sucre, cacao],
	[mais, cacao, cacao],
	[riz,riz,cafe,cacao]
]).
plateau([[[ble,7],[riz,6],[cacao,6],[cafe,6],[sucre,6],[mais,6]], 
	[[mais, riz, ble, ble],
	[ble, mais, sucre, riz],
	[cafe, sucre, cacao, riz],
	[cafe, mais, sucre, mais],
	[cacao, mais, ble, sucre],
	[riz, cafe, sucre, ble],
	[cafe, ble, sucre, cacao],
	[mais, cacao, cacao],
	[riz,riz,cafe,cacao]],
 1, [],[]]).
% positionTrader : entier [1..nb piles non vides]
% inutiles ?
reserveJoueur1([]).
reserveJoueur2([]).
% plateau[Marchandises, Bourse, PositionTrader, ReserveJoueur1, ReserveJoueur2].

%affichagePlateau(+Plateau).
%(1) Affichage des cours de la bourse
%(2) Affichage des piles + position du joueur
%(3) Affichage des reserves des joueurs

%affiche(+ressource)
%ecrit le nom de la ressource
affiche([]):- write('vide').
affiche(ble) :- write('blé  ').
affiche([ble|_]) :- write(' blé ').
affiche(mais) :- write('maïs ').
affiche([mais|_]) :- write('maïs ').
affiche(cacao) :- write('cacao').
affiche([cacao|_]) :- write('cacao').
affiche(riz) :- write(' riz ').
affiche([riz|_]) :- write(' riz ').
affiche(sucre) :- write('sucre').
affiche([sucre|_]) :- write('sucre').
affiche(cafe) :- write('café ').
affiche([cafe|_]) :- write('café ').

%conversion(+code, -ressource)
%transforme le code d'une ressource en son nom
conversion(1, ble).
conversion(2, mais).
conversion(3, cacao).
conversion(4, riz).
conversion(5, sucre).
conversion(6, cafe).

%concat(+L1, +L2, -L3)
%L3 est la concaténée de L1 et L2
concat([], L, L).
concat([H1|T1], L2, [H1|Y]) :- concat(T1, L2, Y).

%longeur(+L, -X)
%X est la taille de la liste L.
longueur([],0).
longueur([_|Q], X):- longueur(Q, X2), X is X2 + 1.

%clean(+Piles, -PilesPropres, +Etat)
%enleve les listes vides
clean([],[], 0).
clean([[]|Q],Q1, Etat):- clean(Q, Q1, Etat2), Etat is Etat2 + 1, !.
clean([X|Q], Q2, Etat):- clean(Q, Q1, Etat), concat([X], Q1, Q2), !.

%remove(+Etat, +Pile, -PileSansTete)
%enleve le sommet de la pile
remove(_, [],[]):-!.
remove(1,[[_|Q]|Q2], Q4):- concat([Q], Q2, Q4), !.
remove(N, [T|Q],Q5):- N2 is N-1, remove(N2, Q, Q3), concat([T], Q3, Q5).


%générerRessource(-Z)
%renvoie dans Z une ressource("riz", "blé",...)
genererRessource(Z) :- random(1,7,X), conversion(X,Z).

%générerListeRessource(-L,+T)
%renvoie dans Z une liste de T ressources("riz", "blé",...)
genererListeRessources(_,0):-!.
genererListeRessources(L,T):- T2 is T-1, genererRessource(R), genererListeRessources(L2,T2),concat(L2,[R], L).

%générerPile(-L,+T).
%génère T piles dans L
genererPiles(_,0):-!.
genererPiles(L, T):- T2 is T-1, genererListeRessources(Z,4), genererPiles(L2, T2),!, concat(L2, [Z], L).

%afficherPlateau(+Plateau)
%affiche le plateau entré en paramètre
afficherPlateau([Bourse, Piles, Joueur, Reserve1, Reserve2]) :-
	afficherBourse(Bourse), write('\n-----------------------------------\n'),
	afficherPiles(Piles), write('\n'), afficherPosition(Joueur),
	write('\n-----------------------------------\n'),
	write('Reserve Joueur 1: '), afficherReserve(Reserve1,1),
	write('\n-----------------------------------\n'),
	write('Reserve Joueur 2: '), afficherReserve(Reserve2,1),
	write('\n-----------------------------------\n').

%afficherBourse(+Bourse)
% affiche toutes les valeurs des ressources
afficherBourse([]).
afficherBourse([X|L]) :- afficherCours(X), write('\n'), afficherBourse(L).

%afficherCours(+Cours)
% affiche une seule ligne de la bourse (la valeur d'une ressource)
afficherCours([X|L]) :- affiche(X), write('\t => \t'), write(L).

%afficherPile(+Pile)
%affiche le sommet de chaque pile
afficherPiles([]).
afficherPiles([X|L]) :- write('('),affiche(X), write(')'), afficherPiles(L).

%afficherPosition(+Position)
%place le pion $
afficherPosition(X) :- A is X-1, avancer(A), write('   $').

%avancer(+X)
%ajuste la place du pion $ avec la pile correspondante
avancer(0) :- !.
avancer(X) :- X2 is X-1, write('       '), avancer(X2).


%afficherReserve(+Reserve, +Booleen)
%affiche la reserve d'un joueur
%On appelle afficherReserve avec 1 en paramètre pour savoir si liste vide lors du premier appel.
afficherReserve([],1) :- write('vide'), !. 
afficherReserve([],0) :- !.
afficherReserve([X|L],_) :- write('('), affiche(X),write(')'), afficherReserve(L,0).

%plateauDepart([-Bourse,-Piles,_,_]).
%affectation de la variable bourse et génération des piles que l'on place dans le plateau
plateauDepart([B,P,1,[],[]]) :- genererPiles(P,9), bourse(B). 

%check(+nombre, +min, +max).
%renvoie 'yes' si Min=<V=<max
check(V, Min, Max):- V =< Max, V >= Min, !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Jeu %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

%start
%demande le type de partie et lance le type correspondant
start :-write('******** Chicago Stock Exchange *********\n\n'),
		write('Type de Partie: \n -1. Humain VS Humain\n -2. Humain VS PC\n -3. PC VS PC\n\n Votre Choix: '),
		read(Type), 
		demandeDificulte(Type).

%demandeDificulte(+typeDePartie)
%demande la diificulté de l'IA avant de lancer la partie lorsque c'est nécessaire
demandeDificulte(X) :- check(X, 2, 3), write('\n\nDificulté: \n -1. Facile\n -2. Moyen\n -3. Difficile \n\n Votre Choix: '), read(Dificulte), launcher(X, Dificulte), !.
demandeDificulte(X):- X==1, launcher(X, 1), !.
demandeDificulte(_):-write('\n\n\n'), start.

%launcher(+Type, +Dificulté).
%lance le type de partie désiré
launcher(X,Y):-check(Y,1,3), plateauDepart(Plateau), game(Plateau, X, Y2), !.
launcher(X,_):-write('\n\n\n'), demandeDificulte(X).

%game(+Plateau,+X,+Dificulte)
%boucle principale qui représente un tour du jeu
game(Plateau, 1, _):- afficherPlateau(Plateau), 
				   playerTurn(Plateau, NouveauPlateau, 1), %J1
				   afficherPlateau(NouveauPlateau), 
				   playerTurn(NouveauPlateau, NouveauPlateau2, 2), %J2
				   game(NouveauPlateau2, 1, Dificulte). %J1 démarre puis J2 puis on boucle
				   
game(Plateau, 2, Dificulte):- afficherPlateau(Plateau), 
				   playerTurn(Plateau, NouveauPlateau, 1), %J1
				   afficherPlateau(NouveauPlateau), 
				   computerTurn(NouveauPlateau, NouveauPlateau2, 2, Dificulte), %COMPUTER
				   game(NouveauPlateau2, 2, Dificulte). %J1 démarre puis CP puis on boucle
game(Plateau, 3, Dificulte):- afficherPlateau(Plateau), 
				   computerTurn(Plateau, NouveauPlateau, 1, Dificulte), %COMPUTER
				   afficherPlateau(NouveauPlateau), 
				   computerTurn(NouveauPlateau, NouveauPlateau2, 2, Dificulte), %COMPUTER
				   game(NouveauPlateau2, 3, Dificulte). %J1 démarre puis CP puis on boucle

%playerTurn(+Plateau, -NouveauPlateau, +Joueur)	  
%Controle le tour du joueur 
playerTurn(Plateau, NouveauPlateau, Joueur):- fin(Plateau), prepare_coup(Plateau, Coup, Joueur), jouer_coup(Plateau, Coup, NouveauPlateau).
%computerTurnn(+Plateau, -NouveauPlateau, +Joueur, +Dificulte)
%controle le tour de l'ordinateur en fonction de la dificulté
computerTurn(Plateau, NouveauPlateau, Joueur, Dificulte):- fin(Plateau), meilleurCoupSup(Plateau, Coup, Dificulte, Joueur, Joueur, Score), jouer_coup(Plateau, Coup, NouveauPlateau).

%fin(+Plateau).
%ecrit les scores lorsque la partie est finie.
fin([_,P,_,_,_]):- longueur(P, L), L > 2, !.
fin([B,_,_,R1,R2]):- write('FIN \n'),nl, 
					write('Score J1: '), score(B, R1, R1, Result1),
					write(Result1),nl,
					write('Score J2: '), score(B, R2, R2, Result2),
					write(Result2),nl,nl,
					felicitations(Result1,Result2),
					nl,nl,
					start.

%score(+Bourse, +reserve, +reserve, -resultat).
%calcule le score d'un joueur
score([[_, _]], [],_, 0):-!.
score([[_, _]|Q], [], Reserve, Result):- score(Q, Reserve, Reserve, Result),  !.
score([[Ressource, Valeur]|Q], [Ressource|Q2], Reserve, Result):- score([[Ressource, Valeur]|Q], Q2,Reserve, R), Result is R + Valeur, !.
score([[Ressource, Valeur]|Q], [_|Q2], Reserve, Result):- score([[Ressource, Valeur]|Q], Q2, Reserve, Result).

%felicitation(+Score1, +Score2).
%ecrit le nom du gagnant
felicitations(Result1, Result2):- Result1 > Result2, write('**** J1 WIN ****'), !.
felicitations(Result1, Result2):- Result1 < Result2, write('**** J2 WIN ****'), !.
felicitations(_,_):- write('**** EGALITE ****').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PREPARATION DU COUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%prepare_coup(+Plateau, +Coup, +Joueur)
%représente un coup dans le jeu.
prepare_coup([_, P, J, _, _], [Joueur, NouvellePosition, X1, X2], Joueur):-
					deplacerPion(J,P, NouvellePosition),
					choixRessources(P, NouvellePosition, Ressources), 
					nth1(1, Ressources, X1), nth1(2, Ressources, X2).

%déplacerPion(+AnciennePostion,+Piles, -NouvellePosition)
%Demande à l'utilisateur son coup et avance le pion
deplacerPion(AnciennePosition,Piles, NouvellePosition):- write('De combien de cases voulez-vous déplacer le pion ? [1..3]\n > '),
					read(N), longueur(Piles, L),
					check(N,1,3),!, % SI N est entre 1 et 3 alors on actualise la nouvelle position du joueur
					Tmp is (AnciennePosition + N) mod L,
					replacerPion(Tmp,L,NouvellePosition).
					
					% SINON ON BOUCLE
deplacerPion(AnciennePosition,Piles,NouvellePosition):- deplacerPion(AnciennePosition,Piles,NouvellePosition).

%replacerPion(+avancée, +nbPiles, -NouvellePosition)
%remet le pion sur la bonne pile
replacerPion(0,L,L):-!.
replacerPion(AnciennePosition,_, AnciennePosition).
	
%choixRessource(+Piles, +Position, -Ressources)
%Ecrit les ressources à conserver ou à rejetter et récupère la réponse du joueur												
choixRessources(Piles, Position, Ressources):-write('Quelle ressource souhaitez-vous conserver ? \n'),
									ressource(Position, Piles, X1, X2),
									write('\n -1. '), nth1(X1, Piles, [T1|Q1]), affiche(T1),
									write('\n -2. '), nth1(X2, Piles, [T2|Q2]), affiche(T2),
									write('\n\n > '), read(N),
									check(N,1,2),!, ecrireRessources(N, T1, T2, Ressources).
									
									
% SINON ON BOUCLE
choixRessources(Piles, Position, Ressources):-choixRessources(Piles, Position, Ressources).

%ressource(+Position,+Piles, -ressourceDessous, -ressourceDessus)
%renvoie les piles adjacentes au pion 
ressource(L, Piles, X1, X2):- longueur(Piles, L), X1 is L-1, X2 is 1, !. 
ressource(1, Piles, X1, X2):- longueur(Piles, L), X1 is L, X2 is 2, !. 
ressource(Position, Piles, X1, X2):- longueur(Piles, L),
									X1 is Position - 1,
									X2 is Position + 1, !.

%ecrireRessource(+Choix, +Ressource1, +Ressource2, -listeRessource)
%renvoie la ressource choisie en tete de liste et la ressource défaussée en fin de liste
ecrireRessources(1, T1, T2, Ressources):- concat([T1], [T2], Ressources),!.
ecrireRessources(_, T1, T2, Ressources):- concat([T2], [T1], Ressources).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% JOUER COUP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Jouer_coup(+ancienPlateau, +Coup, -NouveauPlateau)
%Modifie le plateau en fonction du coup joué
jouer_coup([B, P, J, R1, R2], [X, NouvellePosition, RessourceIN, RessourceOUT], [B2, P2, PositionCorrigee, R12, R22]):-
									updatePile(P, NouvellePosition, PositionCorrigee, P2),
									updateBourse(B, RessourceOUT, B2),
									ajouterRessource(X, R1, R2, RessourceIN, R12, R22).
									
									
%updatePile(+Piles, +Position, -PositionCorigée, -Piles)
%Effectue les changements nécessaires sur les piles après un coup
updatePile(P, NouvellePosition,PositionCorrigee, P2):-
			ressource(NouvellePosition,P,X1,X2),
			remove(X1, P, P12),
			remove(X2, P12, P122),
			clean(P122, P2, Etat),
			correctionPosition(Etat, P2, NouvellePosition, PositionTMP),
			correctionPositionBeforePlayer(Etat, P2, X2, PositionTMP, PositionCorrigee).
				
%correctionPosition(+Etat, +Piles, +PositionJoueur,-NouvellePositionJoueur)
%corrige la position du joueur en fonction des piles supprimées après sa position.
correctionPosition(0,_, NouvellePosition, NouvellePosition):-!.
correctionPosition(_,[X], NouvellePosition, 1):-!.
correctionPosition(_,P, NouvellePosition, PositionCorrigee):- longueur(P, L), 
								Tmp is (NouvellePosition - 1) mod L,	
								replacerPion(Tmp, L, PositionCorrigee).
%correctionPositionBeforePlayer(+Etat, +Piles, +PositionJoueur,-NouvellePositionJoueur)
%corrige la position du joueur en fonction des piles supprimées avant sa position.
correctionPositionBeforePlayer(Etat,P, X2, NouvellePosition, PositionCorrigee):- 
		Etat > 0,
		X2 < NouvellePosition,
		longueur(P, L), 
		Tmp is (NouvellePosition - 1) mod L,	
		replacerPion(Tmp, L, PositionCorrigee),!.

correctionPositionBeforePlayer(_,_,_,NouvellePosition,NouvellePosition).


%updateBourse(+Bourse, +ressource, -Bourse)
%vend une ressource sur la place boursière						
updateBourse([[RessourceOUT,V]|Q], RessourceOUT, [[RessourceOUT,V2]|Q]):- V2 is V - 1, !.
updateBourse([T|Q], RessourceOUT, B2):- updateBourse(Q, RessourceOUT, B12), concat([T], B12, B2).

%ajouterRessource(+Joueur, +resourcesJ1, +ressourcesJ2, +Ressource à ajouter, -NouvelleListej1, -NouvelleListeJ2)
%Met à jour les resources des deux joueurs après un coup.
ajouterRessource(1, R1, R2, RessourceIN, R12, R2):- concat([RessourceIN], R1, R12). %Ressources du J1
ajouterRessource(2, R1, R2, RessourceIN, R1, R22):- concat([RessourceIN], R2, R22). %Ressources du J2
				   
									
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% COMPUTER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
%maxRessource(+Liste, -ressource, -occurences)
%Comte le nombre d'occurences de la ressource qui apparait le plus dans la liste.
maxRessource([],0,0):-!.
maxRessource([T|Q], Result, Tmp):- nbIterations(T,[T|Q],Tmp1), maxRessource(Q,R,Tmp2), Tmp1 >= Tmp2,!, Result = T, Tmp is Tmp1,!.
maxRessource([T|Q], Result, Tmp):- nbIterations(T,[T|Q],Tmp1), maxRessource(Q,R,Tmp2), Tmp1 < Tmp2,!, Result = R, Tmp is Tmp2,!.

%nbIterations(+Ressource, +Liste, -résultat)
%compte le nombre d'occurence d'une ressource dans une liste
nbIterations(_,[],0):-!.                                                                                                     
nbIterations(Ressource, [Ressource|Q], Result):-nbIterations(Ressource, Q, Result2), Result is Result2 + 1, !.
nbIterations(Ressource, [T|Q], Result):-nbIterations(Ressource, Q, Result).

%cmpRessources(-Reserve, +ressource1, +Ressource2, -RessourceMax)
%renvoie la ressource qui aparait le plus dans la reserve entre la 1 et la 2.
cmpRessources(Reserve, R1, R2, Resultat):- nbIterations(R1,Reserve,Result1), nbIterations(R2,Reserve,Result2), Result1 >= Result2, Resultat = R1, !.
cmpRessources(Reserve, R1, R2, Resultat):- nbIterations(R1,Reserve,Result1), nbIterations(R2,Reserve,Result2), Result1 < Result2, Resultat = R2, !.
*/

%coups_possibles(+plateau, -ListeCoups, +Joueur)
%renvoie la liste des coups possibles pour un joueur sur un plateau
coups_possibles([B,P,J,R1,R2], Liste, Joueur):- liste_DeplacementsIA(J,P,Joueur,Deplacements),%déplacements possibles
						choixRessourcesIA(P, Deplacements, Liste).

%meilleur_coup(+plateau, -coup, +Joueur)
%renvoie le coup qui maximise les chances de gagner								
meilleur_coup([B,P,J,R1,R2], Coup, 1):- coups_possibles([B,P,J,R1,R2], Liste, 1),!,
					recherche_meilleur_coup([B,P,J,R1,R2],Liste,1,Coup,Score).
meilleur_coup([B,P,J,R1,R2], Coup, 2):- coups_possibles([B,P,J,R1,R2], Liste, 2),!,
					recherche_meilleur_coup([B,P,J,R1,R2],Liste,2,Coup,Score).											

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PREPARATION DU COUP COMPUTER %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%recherche_meilleur_coup(+plateau, +coupsPossibles, +joueur,-Coup, -score). 
%recherche le meilleur coup à jouer pour la prochaine itération 
recherche_meilleur_coup([B,P,J,R1,R2],[T],_,T,Score):- jouer_coup([B,P,J,R1,R2], T, [B2,P2,J2,R12,R22]), score(B2, R22, R22, Score).

	
recherche_meilleur_coup([B,P,J,R1,R2],[T|Q],1,Coup,Score):- 
						jouer_coup([B,P,J,R1,R2], T, [B2,P2,J2,R12,R22]),
						score(B2, R22, R22, Tmp),
						recherche_meilleur_coup([B,P,J,R1,R2], Q, 1, Coup2, Score2),
						Tmp < Score2,!,
						Score is Tmp,
						Coup = T.

recherche_meilleur_coup([B,P,J,R1,R2],[T|Q],1,Coup,Score):- 
						jouer_coup([B,P,J,R1,R2], T, [B2,P2,J2,R12,R22]),
						score(B2, R22, R22, Tmp),
						recherche_meilleur_coup([B,P,J,R1,R2], Q, 1, Coup2, Score2),
						Tmp >= Score2,!,
						Score is Score2,
						Coup = Coup2.
						
recherche_meilleur_coup([B,P,J,R1,R2],[T|Q],2,Coup,Score):- 
						jouer_coup([B,P,J,R1,R2], T, [B2,P2,J2,R12,R22]),
						score(B2, R12, R12, Tmp),
						recherche_meilleur_coup([B,P,J,R1,R2], Q, 2, Coup2, Score2),
						Tmp < Score2,!,
						Score is Tmp,
						Coup = T.
						
recherche_meilleur_coup([B,P,J,R1,R2],[T|Q],2,Coup,Score):- 
						jouer_coup([B,P,J,R1,R2], T, [B2,P2,J2,R12,R22]),
						score(B2, R12, R12, Tmp),
						recherche_meilleur_coup([B,P,J,R1,R2], Q, 2, Coup2, Score2),
						Tmp >= Score2,!,
						Score is Score2,						
						Coup = Coup2.
						

%liste_DeplacementsIA(+position, +Piles, +Joueur, -Deplacements)
%recherche une liste des déplacements possibles
liste_DeplacementsIA(J, P, Joueur, Deplacements):- 
					deplacerPionIA(J, P, P1,1),
					deplacerPionIA(J, P, P2,2),
					deplacerPionIA(J, P, P3,3),
					concat([[Joueur,P1]], [[Joueur,P1]], P11),
					concat([[Joueur,P2]], [[Joueur,P2]], P22),
					concat([[Joueur,P3]], [[Joueur,P3]], P33),
					concat(P11, P22, P1122),
					concat(P1122, P33, Deplacements). %On double tous les deplacements, car 2 choix de ressource par deplacement

%deplacerPionIA(+AnciennePosition,+Piles, -NouvellePosition, +Indice)
%simule un déplacement de Indice sur les piles.					
deplacerPionIA(AnciennePosition,Piles, NouvellePosition, Indice):-
					longueur(Piles, L),
					Tmp is (AnciennePosition + Indice) mod L,
					replacerPion(Tmp,L,NouvellePosition).
%choixRessourcesIA(+Piles, +Deplacements, -ListeCoups)
%récupère une liste de déplacement et la transforme en liste de coups.
choixRessourcesIA(_,[],[]).
choixRessourcesIA(P, [T1,T2|Q], Result):- 
					choixRessourcesIA(P, Q, Tmp),
					choixIA(P, T1, 1, R1),
					choixIA(P, T2, 2, R2),
					concat([R1],[R2], Tmp2),
					concat(Tmp2, Tmp, Result), !.
%nth1(-Position,+Liste, +X)
%renvoie la position de X dans la liste 						
nth1(1,[X|_],X) :- !.
nth1(Idx,[_|List],X) :-
    Idx > 1,
    Idx1 is Idx-1,
    nth1(Idx1,List,X).

%choixIA(+Piles, +Deplacement, +choixRessource, -coups)
%renvoie les coups à jouer en fonction des déplacements
choixIA(P,[Joueur, Position], 1, [Joueur, Position, X1, X2]):- ressource(Position, P, PositionR1, PositionR2),
															nth1(PositionR1, P, [X1|Q1]),
															nth1(PositionR2, P, [X2|Q2]), 
															!.

													
choixIA(P,[Joueur, Position], 2, [Joueur, Position, X2, X1]):- ressource(Position, P, PositionR1, PositionR2),
																nth1(PositionR1, P, [X1|Q1]),
																nth1(PositionR2, P, [X2|Q2]).

%maxList(+ListedeCoups, -meilleurCoup, -meilleurScore)
%renvoie le meilleur coup et le meilleur score d'une liste
maxList([[X,Y]],X,Y).
maxList([[Coup, Score]|Q], X, Y) :- 
	maxList(Q,X2,Y2),
	Score >= Y2, !,
	Y = Score,
	X = Coup.

maxList([[Coup, Score]|Q], X, Y) :- 
	maxList(Q,X2,Y2),
	Score < Y2, !,
	Y = Y2,
	X = X2.

%minList(+ListedeCoups, -CoupMinimal, -ScoreMinimum)
%renvoie le coup avec le score minimal parmi une liste
minList([[X,Y]],X,Y).
minList([[Coup, Score]|Q], X, Y) :- 
	minList(Q,X2,Y2),
	Score =< Y2, !,
	Y = Score,
	X = Coup.

minList([[Coup, Score]|Q], X, Y) :- 
	minList(Q,X2,Y2),
	Score > Y2, !,
	Y = Y2,
	X = X2.

%autreJoueur(+joueur, -joueurSuivant)
%renvoie le prochain joueur à jouer
autreJoueur(1,2).
autreJoueur(2,1).

%meilleurCoupSup(+Plateau, -Coup, +Difficulte, +JoueurInitial, +Joueur, -Score)
%renvoie le meilleur coup à jouer pour une IA en fonction de la difficulté du jeu
%plus la dificulté est grande et plus l'IA sera prévoyante.

%meilleurCoupSup(+Plateau, -Coup, +difficulté, +Joueur, -Score).
%Recherche en profondeur le meilleur coup à jouer. La difficulté du jeu est la profondeur de recherche. 
meilleurCoupSup(Plateau, Coup, 0, Joueur, Joueur, Score):-
%appel avec difficulté 0, on ne veut pas chercher plus loin : condition d'arrret
		coups_possibles(Plateau, [A,B,C,D,E,F], Joueur),
		jouer_coup(Plateau, A,[B1,P1,J1,R11,R21]), 
		jouer_coup(Plateau, B,[B2,P2,J2,R12,R22]), 
		jouer_coup(Plateau, C,[B3,P3,J3,R13,R23]), 
		jouer_coup(Plateau, D,[B4,P4,J4,R14,R24]), 
		jouer_coup(Plateau, E,[B5,P5,J5,R15,R25]), 
		jouer_coup(Plateau, F,[B6,P6,J6,R16,R26]),
		score(B1,R11,R11,Resultat1),
		score(B2,R12,R12,Resultat2),
		score(B3,R13,R13,Resultat3),
		score(B4,R14,R14,Resultat4),
		score(B5,R15,R15,Resultat5),
		score(B6,R16,R16,Resultat6),
		Temp = [[A,Resultat1],[B,Resultat2],[C,Resultat3],[D,Resultat4],[E,Resultat5],[F,Resultat6]],
		maxList(Temp, Coup, Score), !.

meilleurCoupSup(Plateau, Coup, 0, _, Joueur, Score):-
		coups_possibles(Plateau, [A,B,C,D,E,F], Joueur),
		jouer_coup(Plateau, A,[B1,P1,J1,R11,R21]), 
		jouer_coup(Plateau, B,[B2,P2,J2,R12,R22]), 
		jouer_coup(Plateau, C,[B3,P3,J3,R13,R23]), 
		jouer_coup(Plateau, D,[B4,P4,J4,R14,R24]), 
		jouer_coup(Plateau, E,[B5,P5,J5,R15,R25]), 
		jouer_coup(Plateau, F,[B6,P6,J6,R16,R26]),
		score(B1,R21,R21,Resultat1),
		score(B2,R22,R22,Resultat2),
		score(B3,R23,R23,Resultat3),
		score(B4,R24,R24,Resultat4),
		score(B5,R25,R25,Resultat5),
		score(B6,R26,R26,Resultat6),
		Temp = [[A,Resultat1],[B,Resultat2],[C,Resultat3],[D,Resultat4],[E,Resultat5],[F,Resultat6]],
		minList(Temp, Coup, Score), !.


meilleurCoupSup(Plateau, Coup, Difficulte, Joueur, Joueur, Score) :- 
%c'est au joueur qui a appelé le prédicat la première fois de jouer : on maximise son score.
		Dif is Difficulte - 1,fin(Plateau),
		coups_possibles(Plateau, [A,B,C,D,E,F], Joueur),
		jouer_coup(Plateau, A,NouveauPlateauA), 
		jouer_coup(Plateau, B,NouveauPlateauB), 
		jouer_coup(Plateau, C,NouveauPlateauC), 
		jouer_coup(Plateau, D,NouveauPlateauD), 
		jouer_coup(Plateau, E,NouveauPlateauE), 
		jouer_coup(Plateau, F,NouveauPlateauF),
		autreJoueur(Joueur,X),
		meilleurCoupSup(NouveauPlateauA, CoupA, Dif, Joueur, X, ScoreA),
		meilleurCoupSup(NouveauPlateauB, CoupB, Dif, Joueur, X, ScoreB),
		meilleurCoupSup(NouveauPlateauC, CoupC, Dif, Joueur, X, ScoreC),
		meilleurCoupSup(NouveauPlateauD, CoupD, Dif, Joueur, X, ScoreD),
		meilleurCoupSup(NouveauPlateauE, CoupE, Dif, Joueur, X, ScoreE),
		meilleurCoupSup(NouveauPlateauF, CoupF, Dif, Joueur, X, ScoreF),
		Temp = [[A,ScoreA],[B,ScoreB],[C,ScoreC],[D,ScoreD],[E,ScoreE],[F,ScoreF]],
		maxList(Temp, Coup, Score),!.
 


meilleurCoupSup(Plateau, Coup, Difficulte, JoueurInitial, Joueur, Score) :- 
%ce n'est pas au joueur qui a appelé le prédicat la première fois de jouer : on minimise son score.
		Dif is Difficulte -1,fin(Plateau),
		coups_possibles(Plateau, [A,B,C,D,E,F], Joueur),
		jouer_coup(Plateau, A,NouveauPlateauA), 
		jouer_coup(Plateau, B,NouveauPlateauB), 
		jouer_coup(Plateau, C,NouveauPlateauC), 
		jouer_coup(Plateau, D,NouveauPlateauD), 
		jouer_coup(Plateau, E,NouveauPlateauE), 
		jouer_coup(Plateau, F,NouveauPlateauF),
		autreJoueur(Joueur,X),
		meilleurCoupSup(NouveauPlateauA, CoupA, Dif, Joueur, X, ScoreA),
		meilleurCoupSup(NouveauPlateauB, CoupB, Dif, Joueur, X, ScoreB),
		meilleurCoupSup(NouveauPlateauC, CoupC, Dif, Joueur, X, ScoreC),
		meilleurCoupSup(NouveauPlateauD, CoupD, Dif, Joueur, X, ScoreD),
		meilleurCoupSup(NouveauPlateauE, CoupE, Dif, Joueur, X, ScoreE),
		meilleurCoupSup(NouveauPlateauF, CoupF, Dif, Joueur, X, ScoreF),
		Temp = [[A,ScoreA],[B,ScoreB],[C,ScoreC],[D,ScoreD],[E,ScoreE],[F,ScoreF]],
		minList(Temp, Coup, Score),!.

meilleurCoupSup([B,P,J,R1,R2], Coup,_, _, Joueur, _) :- 
	meilleur_coup([B,P,J,R1,R2], Coup, Joueur), !,%en cas de fin, on utilise le meilleur coup.


