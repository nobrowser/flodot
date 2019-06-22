SHELL = /bin/sh
BASE_PATH := $(shell printenv PATH)
OPAM_PATH := /usr/local/packages/opam/$(shell opam switch show)/bin
FP := env PATH=$(BASE_PATH):$(OPAM_PATH)

byte:
	$(FP) ocamlbuild -classic-display -use-ocamlfind Flograph.cma

clean:
	$(FP) ocamlbuild -classic-display -use-ocamlfind -clean

.PHONY: byte clean

