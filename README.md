# Sudoku
PFA's Project


CryptPad : https://cryptpad.fr/code/#/2/code/edit/E8dXYmBdhzwAAD+c35lGBmeR/

20/01/2020 cryptpad https://cryptpad.fr/code/#/2/code/edit/nAkWCO6yoNgfsWz1JMJHkI-I/

# Objectifs

### Fonctionnalités de base

<ul>
  <li> [x] Il faut charger une grille de Sudoku et l'afficher comme dans l'image en haut
    Cette grille doit être chargée à partir d'un fichier</li>
  <li> [ ] Un joueur doit pouvoir cliquer sur une case de la grille et la remplir avec un chiffre entre 1 et 9
on ne doit pas pouvoir cliquer et modifier une case préremplie</li>
  <li> [x] Le jeu doit vérifier si les réponses sont correctes ou pas. Vous êtes libres dans le choix de vérifier la grille en entière ou vérifier case par case.</li>
  <li> [x] Dès que la grille est complète, le jeu doit indiquer si vous avez gagné ou perdu</li>
  <li> [ ] Le joueur doit être capable de revenir en arrière et effacer le(s) dernier(s) chiffre(s) ajouté(s)</li>
  <li> [ ] Le joueur doit pouvoir demander de l'aide au jeu pour résoudre la grille</li>
  <li> [ ] On doit pouvoir enregistrer et charger une partie</li>
  <li> [ ] On doit avoir une bouton qui affiche la liste des commandes et les règles du Sudoku</li>
</ul>

### Fonctionnalités supplémentaires

<ul>
  <li> [ ] Backtracking : stocker chaque décision dans une pile. Si vous voulez annuler une décision et effacer le chiffre, votre jeu doit effacer tous les decisions qui ont été faites après la decision à effacer</li>
  <li> [ ] Ajouter des niveaux de difficulté</li>
  <li> [ ] Menu: avoir un menu pour :
démarrer le jeu / charger une partie | 
choisir le niveau de difficulté | 
choisir une grille (avec un aperçu de chaque grille)</li>
  <li> [ ] Ajouter un timer pour le jeu</li>
  <li> [ ] Possibilité de faire pause pour arrêter le timer
masquer la grille si le timer est arrêté</li>
  <li> [ ] Calculer un score pour chaque grille en fonction du temps
garder le score pour pouvoir essayer de le battre</li>
  <li> [ ] Surligner la case sélectionnée</li>
  <li> [ ] Améliorer l'affichage (ajouter un fond, etc)</li>
  <li> [ ] Redémarrer une partie</li>
  <li> [ ] Agmenter les fonctionnalités de l'aide au joueur
donner plusieurs possibilités pour chaque case | 
limiter le nombre de fois qu'un joueur peut demander de l'aide | 
cliquer sur un chiffre surligne ce chiffre partout dans la grille</li>
  <li> [ ] Un jeu complet : le jeu est composé de plusieurs niveaux, s'enchaînant les uns avec les autres. Il y a une logique de jeu (score pour chaque niveau etc)</li>
  <li> [ ] Vous êtes libres de rajouter d'autres fonctionnalités dans votre jeu tant que vous les expliquez dans le rapport</li>
</ul>


# Notes


1er mode : Méthode création aléatoire de grille sans système de vie avec retour en arrière, et vérification seulement si les règles du jeu sont respectées. En revanche, rien ne nous dit si la grille est faisable ou non. (voir avec sat solver)

2ème mode : Méthode avec système de vie et vérification avec la grille de solution à chaque fois que l'on souhaite placer un nombre. Dans ce mode, il y aura un mode de vie et pas de retour en arrière

3ème mode : Méthode sans système de vie avec retour en arrière et vérification avec la grille de solution seulement à la fin quand la grille sera complètement remplie, à ce moment-là le système nous dira si la grille de sudoku est correcte ou non avec l'aide de la correction.











# MakeFile


all: sudoku.native

%.native: *.ml
	ocamlbuild -use-ocamlfind -pkg graphics $@

.PHONY: clean

clean:
	ocamlbuild -clean
