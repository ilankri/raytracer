SHELL = /bin/sh
OCAMLBUILD = ocamlbuild -use-ocamlfind
TARGET = ray

.SUFFIXES:
.PHONY: all opt byte repl doc clean

all: opt

# The tag unsafe is used to speed up array accesses.
opt:
	$(OCAMLBUILD) -tag unsafe $(TARGET).native

byte:
	$(OCAMLBUILD) -tag debug,bin_annot $(TARGET).byte

repl: byte
	rlwrap ocaml -init $(TARGET).top

doc:
	$(OCAMLBUILD) $(TARGET).docdir/index.html

clean:
	$(OCAMLBUILD) -clean
