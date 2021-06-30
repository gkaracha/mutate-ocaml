all: build test run

run:
	dune exec -- src/main/main.exe "tests/example.ml" # EXAMPLE. TODO: file as input (and output)

build:
	dune build @install

test:
	dune runtest .

clean:
	$(RM) -r _build *.cmi *.cmx *.o

distclean: clean

.PHONY: all build test clean distclean
