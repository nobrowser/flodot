SHELL = /bin/sh
PATH != eval "`opam env`" ; echo "$${PATH}"
export PATH

PKGS = \
	-package cmdliner \
	-package aaa \
	-package ppx_deriving.std \
	-package ocamlgraph \
	-package yojson

OCAMLC = ocamlfind ocamlc $(PKGS) -linkpkg -principal   # add other options for ocamlc here
OCAMLOPT = ocamlfind ocamlopt $(PKGS) -linkpkg -principal

# The list of object files for FLODOT
FLODOT_SUBMODULES != echo [A-Z]*.ml
FLODOT_INTERFACES = $(FLODOT_SUBMODULES:.ml=.mli)

FLODOT_NATIVEOBJS != ./linkorder.sh native $(FLODOT_SUBMODULES) $(FLODOT_INTERFACES)
FLODOT_BYTEOBJS != ./linkorder.sh byte $(FLODOT_SUBMODULES) $(FLODOT_INTERFACES)

.PHONY: byte native

byte: flodot.byte

native: flodot.native

flodot.byte: flodot.cmo $(FLODOT_BYTEOBJS)
		$(OCAMLC) -o flodot.byte $(FLODOT_BYTEOBJS) flodot.cmo

flodot.native: flodot.cmx $(FLODOT_NATIVEOBJS)
		$(OCAMLOPT) -o flodot.native $(FLODOT_NATIVEOBJS) flodot.cmx

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
		$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

.ml.cmx:
		$(OCAMLOPT) $(OCAMLFLAGS) -c -o $@ $<

.mli.cmi:
		$(OCAMLC) $(OCAMLFLAGS) -c -o $@ $<

# Dependencies
.depend: Makefile $(FLODOT_INTERFACES) $(FLODOT_SUBMODULES) flodot.ml
		ocamldep $(FLODOT_INTERFACES) $(FLODOT_SUBMODULES) flodot.ml > .depend

include .depend

.PHONY: clean

clean:
		rm -f *.cmo *.o *.cmx *.cmi *.byte *.native .depend
