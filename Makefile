all: build test

build:
	dune build @install

test:
	dune runtest .

clean:
	$(RM) -r _build *.cmi *.cmx *.o

distclean: clean
	$(RM) parser

.PHONY: all build test clean distclean
