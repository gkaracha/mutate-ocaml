
build:
	ocamlopt -I +compiler-libs ocamlcommon.cmxa -o parser experiment.ml

clean:
	$(RM) -r *.cmi *.cmx *.o

distclean: clean
	$(RM) parser
