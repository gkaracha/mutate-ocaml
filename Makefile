all: build test

run:
	dune exec -- src/main/main.exe "src/mutator/mutator.ml" # EXAMPLE. TODO: file as input (and output)

build:
	dune build @install

test:
	dune runtest .

clean:
	$(RM) -r _build *.cmi *.cmx *.o

distclean: clean
	$(RM) parser

.PHONY: all build test clean distclean
