# [Projet Ocaml](https://github.com/L3-Info-Miage-Universite-Cote-D-Azur/projetinfo-2019-stoneage-sad)


> Repository for the L3 Project in Functionnal Programming


[![Release](https://img.shields.io/badge/Current_Release-None-blue.svg)](https://github.com/L3-Info-Miage-Universite-Cote-D-Azur/projetinfo-2019-stoneage-sad/releases/latest)
[![Java](https://img.shields.io/badge/Ocaml-4.07.0-orange.svg?logo=ocaml&logoColor=orange)](https://www.oracle.com/technetwork/java/javase/downloads/jdk11-downloads-5066655.html)

## Description
>Le projet a pour but la réalisation d'un petit jeu de Pacman en Ocaml.

### Lien du projet : 
https://lms.univ-cotedazur.fr/mod/url/view.php?id=189570

---



### Utilisation : 
1. Télécharger le projet
2. Extraire le projet
3. ocamlc -I ~/.opam/default/lib/graphics  graphics.cma UF.ml labyrinthe.ml show.ml -o run.byte && ./run.byte

---

[![Version](https://img.shields.io/badge/Etape-1-blue.svg?style=for-the-badge&logo=appveyor)](https://github.com/L3-Info-Miage-Universite-Cote-D-Azur/projetinfo-2019-stoneage-sad/milestone/1)
![Deadline](https://img.shields.io/badge/DEADLINE-30/11/2020-green.svg?style=for-the-badge&logo=codeforces)
#### Afficher le Labyrinthe
***Les enjeux de cette partie sont :***
* La représentation des données.
* La création d'un type UF.
* La construction d'un tableau de booleens représentant le labyrinthe.
* Afficher à l'aide de Ocaml Graphics le labyrinthe.

***Description :***

Dans cette partie il faut générer le labyrinthe puis l'afficher.
Il faudra utiliser le type de donnée Union Find afin de créer un labyrinthe complet (1 chemin de l'entrée à la sortie)
Puis afficher le labyrinthe ainsi obtenu grâce à la librairie Graphics d'Ocaml

---

[![Version](https://img.shields.io/badge/ETAPE-2-2578B0.svg?style=for-the-badge&logo=appveyor)]()
![Deadline](https://img.shields.io/badge/DEADLINE-06/12/2020-green.svg?style=for-the-badge&logo=codeforces)
#### Interaction Utilisateur
***Fonctionnalités attendues :***
* Afficher un Pacman
* Pouvoir Bouger le Pacman grâce aux saisies clavier
* Actualiser l'affichage en conséquence
* Gagner la partie (le pacman arrive à la fin du labyrinthe)

***Description :***

Dans cette partie du projet il faut créer un pacman (rond jaune) et être en mesure de le déplacer dans le labyrinthe.
Pour réaliser le déplacement on utilisera les saisies claviers et il faudra mettre à l'affichage pour que le pacman ne 
reste pas affiché à sa position initiale après un déplacement.
Si le pacman atteint la fin du labyrinthe le joueur gagne. (il faut donc déterminer la dernière case et le fait 
que le pacman l'atteigne.)

---
[![Version](https://img.shields.io/badge/Etape-3-blue.svg?style=for-the-badge&logo=appveyor)]()
![Deadline](https://img.shields.io/badge/DEADLINE-13/12/2020-blue.svg?style=for-the-badge&logo=codeforces)
#### Fantôme
***Fonctionnalités attendues :***
* Afficher un fantôme
* Faire en sorte que le fantôme poursuive/se dirige vers Pacman
* Perdre la partie (si le Pacman est touché par le fantôme)

***Description :***

Dans cette partie du projet il faut implémenter le fantôme (rond bleu), ennemi juré de Pacman, qui tentera de rattraper 
Pacman et de l'empêcher d'atteindre le bout du labyrinthe. Il faudra donc coder un 'comportement'. Si le fantôme atteint le 
Pacman la partie s'arrête et le joueur a perdu.
