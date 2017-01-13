## Quelques possibilités d'options supplémentaires pour ocamlbuild:
## -pkg xxx : pour utiliser d'autres bibiliothèques
##            (p.ex. unix, functory, parmap, ...)
## -cflag -unsafe : pour accélerer les accès aux tableaux
##                  une fois que le code fonctionne bien

OPTS=-use-ocamlfind -use-menhir -pkg graphics

opt:
	ocamlbuild $(OPTS) ray.native

byte:
	ocamlbuild $(OPTS) -tag debug ray.byte

doc:
	ocamlbuild ray.docdir/index.html

clean:
	ocamlbuild -clean
