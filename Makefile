SHELL = /bin/sh
PATH != eval "`opam env`" ; echo "$${PATH}"
export PATH

OCAMLFLAGS = -principal

# The list of object files for FLODOT
FLODOT_SUBMODULES != echo [A-Z]*.ml
FLODOT_INTERFACES = $(FLODOT_SUBMODULES:.ml=.mli)

FLODOT_NATIVEOBJS != ./linkorder.sh native $(FLODOT_SUBMODULES) $(FLODOT_INTERFACES)
FLODOT_BYTEOBJS != ./linkorder.sh byte $(FLODOT_SUBMODULES) $(FLODOT_INTERFACES)

.PHONY: byte native

byte: flodot.byte

native: flodot.native

flodot.byte: flodot.cmo $(FLODOT_BYTEOBJS)
		@ ./compile.sh byte -linkpkg flodot.byte $(OCAMLFLAGS) $(FLODOT_BYTEOBJS) flodot.cmo

flodot.native: flodot.cmx $(FLODOT_NATIVEOBJS)
		@ ./compile.sh native -linkpkg flodot.native $(OCAMLFLAGS) $(FLODOT_NATIVEOBJS) flodot.cmx

# Common rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
		@ ./compile.sh byte -c $@ $(OCAMLFLAGS) $<

.ml.cmx:
		@ ./compile.sh native -c $@ $(OCAMLFLAGS) $<

.mli.cmi:
		@ ./compile.sh byte -c $@ $(OCAMLFLAGS) $<

# Dependencies
.depend: Makefile $(FLODOT_INTERFACES) $(FLODOT_SUBMODULES) flodot.ml
		ocamldep $(FLODOT_INTERFACES) $(FLODOT_SUBMODULES) flodot.ml > .depend

include .depend

.PHONY: clean

clean:
		rm -f *.cmo *.o *.cmx *.cmi *.byte *.native .depend
