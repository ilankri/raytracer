## Quelques possibilités d'options supplémentaires pour ocamlbuild:
## -cflag -unsafe : pour accélerer les accès aux tableaux
##                  une fois que le code fonctionne bien

SHELL = /bin/sh

OPTS = -use-ocamlfind -use-menhir -pkg graphics

.SUFFIXES:
.PHONY: opt byte doc clean

opt:
	ocamlbuild $(OPTS) ray.native

byte:
	ocamlbuild $(OPTS) -tag debug ray.byte

doc:
	ocamlbuild ray.docdir/index.html

clean:
	ocamlbuild -clean
