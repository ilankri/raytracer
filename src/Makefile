
## Quelques possibilités d'options supplémentaires pour ocamlbuild:
## -pkg xxx : pour utiliser d'autres bibiliothèques
##            (p.ex. unix, functory, parmap, ...)
## -cflag -unsafe : pour accélerer les accès aux tableaux
##                  une fois que le code fonctionne bien

OPTS=-use-ocamlfind -use-menhir -pkg graphics

opt:
	ocamlbuild $(OPTS) main.native

byte:
	ocamlbuild $(OPTS) -tag debug main.byte

clean:
	ocamlbuild -clean

