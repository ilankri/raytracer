Ray-tracing : imagerie 3D par lancer de rayon
=============================================

**Projet 2016 de Programmation Fonctionnelle Avancée**

Le *lancer de rayons* (ou *ray-tracing* en anglais) est un algorithme
qui permet de créer des images réalistes en utilisant une idée assez
simple: pour chaque point de l'image, on calcule la lumière qui y
arrive en utilisant les équations décrivant les propriétés optiques de
la lumière et des objets.

On décrit les objets présents dans la scène à représenter, les
différentes sources de lumières et la position de l'observateur, le
programme se charge de calculer l'image correspondante.

Si vous avez accès à l'internet vous pouvez voir quelques exemples
d'images impressionnantes obtenues avec une bonne implantations du
lancer de rayon (POVRAY) sur le site http://www.povray.org. Dans ce
projet, nous allons réaliser une implantation plus modeste, mais qui
permet toutefois d'obtenir de bonnes images.

### Travail demandé ###

A faire d'ici à **Dimanche 8 Janvier 2016 23:59**, par groupe d'au plus 3 étudiants.

* **Implémenter les notions géométriques nécessaires**. Vous trouverez
  plus de détails dans le fichier de référence
  [doc/objects.pdf](doc/objects.pdf).  En particulier, à vous de choisir
  une représentation des objets géométriques aussi adaptée que possible
  au calcul des lancers de rayons. Certaines interfaces de modules vous
  sont déjà données, p.ex.  [src/vect.mli](src/vect.mli). Merci de ne
  pas modifier ces interfaces sans notre accord. Par contre les types
  dans ces interfaces étant abstraits, vous pourrez les implémenter
  à votre guise.

* **Convertir les scénarios en scène prête à être tracée**. Le format
  de fichier scénario est une description de haut niveau d'une scène,
  et peut contenir en particulier des abréviations ou des appels
  de procédure de construction. Plus de détails dans le fichier
  [doc/scenarios.pdf](doc/scenarios.pdf). Des exemples de scénarios
  sont fournis dans [examples/](examples/). Nous vous fournissons
  le lexeur et parseur permettant d'obtenir une représentation OCaml
  de ces scénarios. A vous de convertir ensuite ces scénarios
  vers vos représentations d'objets géométriques.

* **Obtenir au final des images ou films**. Au minimum, votre programme devra
  pouvoir afficher l'image calculée, ou la stocker dans un fichier PPM
  (cf fichier fourni [src/ppm.mli](src/ppm.mli)).

* **Rapport**: votre archive devra contenir un rapport succinct présentant
  les fonctionnalités de votre programme, les difficultés rencontrées,
  les solutions apportées, etc.


### Prérequis logiciels ###

Pour compiler le code fourni, vous aurez besoin de :

* ocamlbuild
* ocamlfind (paquet debian ocaml-findlib)
* menhir

Pour l'affichage "en direct" de l'image calculée, la bibliothèque
"graphics" fournie par défaut avec OCaml sera bien suffisante.

Pour la sauvegarde de l'image calculée vers un fichier image,
nous fournissons un petit module permettant la création d'images
au format PPM (cf [src/ppm.mli](src/ppm.mli)). Ce format est reconnu ensuite
par la plupart des outils d'affichage d'image ou de conversion,
par exemple "convert" pourra en faire un jpg (paquet debian imagemagick).

Pour la réalisation de films d'animations (cf plus bas), vous pourrez
utiliser l'outil "ffmpeg". Le paquet debian s'appelle ffmpeg, s'il est
disponible (p.ex. pour Debian Jessie, regarder dans jessie-backport),
sinon essayer avec "avconv" du paquet libav-tools.
Il est éventuellement possible d'utiliser "convert" (déjà mentionné)
pour rassembler les images en un (gros) gif animé.

Pour paralléliser votre raytracer (cf plus bas), vous pourrez utiliser
au choix les bibliothèques "parmap" (paquet debian libparmap-ocaml-dev)
et/ou "functory" (disponible via opam).

Si vous souhaitez utiliser tout autre outil ou bibliothèque dans votre
code, demandez-le nous à l'avance, nous statuerons au cas par cas.


### Remarques divers ###

#### Options du programme ####

Votre programme devra accepter au minimum les options suivantes sur
la ligne de commande (en utilisant par exemple le module `Arg` d'OCaml):

 * `-hsize <x>` et `-vsize <y>` pour préciser largeur et hauteur de l'image
 * `-depth <d>` pour préciser le nombre max de rebonds lors du lancer de rayon
 * `-anim <n>` pour préciser que l'on souhaite créer un film à `n` images
   au lieu d'une simple image (cf la variable `time` dans les scénarios).

#### Découpage modulaire ####

Les premiers fichiers fournis (Color, Vect, ...) montrent la voie, à
vous de continuer à architecturer votre projet en différents modules

#### Interface pure ####

Oui, [src/scenario.mli](src/scenario.mli) n'a pas de fichier .ml associé !
C'est voulu, il s'agit d'une pure interface, contenant uniquement des types,
et pas de code. Ne pas créer de scenario.ml !

#### Choix d'implémentation ####

Par exemple, pour les couleurs, les vecteurs, les rotations, etc,
vous pouvez choisir l'implémentation que vous préférez: triplets,
records, tableaux... Vous pourrez même en changer tardivement vu que
ces types sont abstraits dans les interfaces correspondantes.
Quelques informations intéressantes à ce propos:

https://ocaml.org/learn/tutorials/performance_and_profiling.html#Floats

Si vous utilisez des tableaux, quand votre programme sera parfaitement
au point (et uniquement à ce moment-là), vous pourrez gagner
légèrement en performance en compilant avec -unsafe, cf le Makefile.

#### Animation ####

Dès que l'on sait fabriquer une image, il n'y a plus qu'à en faire
plusieurs... Nos scénarios permettent de décrire des animations, où
certains paramètres peuvent évoluer d'image en image : position/taille
de certains objets, lumières, etc. Ceci est contrôlé par la variable
spéciale "time": si elle est utilisée dans le scénario sans y être
définie, elle contiendra le numéro de l'image en cours de rendu.

Ensuite on assemble des différentes images produites en une unique
vidéo, par exemple via l'outil "ffmpeg".  Par exemple, la commande
suivante rassemble toutes les images de la forme truc01234.ppm en une
vidéo au format mp4:

```
ffmpeg -y -r 25 -i truc%05d.ppm truc.mp4
```

Il est éventuellement possible d'utiliser "convert" (déjà mentionné)
pour rassembler les images en un (gros) gif animé.

#### Parallélisme ####

Le raytracing est l'exemple typique d'application qui se parallélisent
très bien. Lors de la création d'un film d'animation, pas besoin d'avoir
fini le rendu d'une image pour s'occuper des suivantes. Même lors
du rendu d'une unique image, on peut calculer en parallèle chaque pixel.
Vous êtes donc très fortement incités à utiliser une des libraries
Parmap ou Functory (ou éventuellement de simples `Unix.fork`) pour
profiter des multiples cores de vos machines (chez vous ou à l'ufr).


#### Analyse de performance ####

Voir par exemple: http://ocaml.org/learn/tutorials/performance_and_profiling.html#Profiling

Apparement on obtient déjà des informations intéressantes via un simple:

```
perf record -g ./ray.native -tofile truc scenario.ray
perf report -g
```

L'outil `perf` fait partie du paquet debian linux-tools

#### Lien avec d'autres rendus 3D ####

Comme extension possible, vous pouvez traduire les scènes 3D
de ce projet en une description utilisable par d'autres outils
de raytracing, par exemple povray, et comparer le rendu obtenu.
Vous pouvez également traduire nos scènes vers une description
OpenGL, et comparer le rendu que l'on obtient alors sur une carte
graphique récente. Voir par exemple la bibliothèque lablgl
(paquet debian liblablgl-ocaml-dev).
