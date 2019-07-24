SHELL = /bin/sh
BASE_PATH := $(shell printenv PATH)
OPAM_PATH := /usr/local/packages/opam/$(shell opam switch show)/bin
COMPILE := env PATH=$(BASE_PATH):$(OPAM_PATH) ocamlbuild -classic-display -use-ocamlfind

byte: Flodot_check.byte Flodot_dot.byte

Flodot_dot.byte: FORCE
	$(COMPILE) Flodot_dot.byte

Flodot_check.byte: FORCE
	$(COMPILE) Flodot_check.byte

FORCE:

clean:
	$(COMPILE) -clean

.PHONY: byte clean
