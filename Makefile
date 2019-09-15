SHELL = /bin/sh
DUNE = dune
TARGET = ray

.SUFFIXES:
.PHONY: all opt byte repl clean

all: opt

opt:
	$(DUNE) build src/$(TARGET).exe

byte:
	$(DUNE) build src/$(TARGET).bc

repl: byte
	rlwrap ocaml -init $(TARGET).top

clean:
	$(DUNE) clean
