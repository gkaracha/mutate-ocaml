# mutate-ocaml
Toy mutating tool for OCaml programs.

I've been unable to get the compiler's internals to work with dune, so I
resorted to old-fashioned `ocamlopt` instead:

```console
$ ocamlopt -I +compiler-libs ocamlcommon.cmxa -o parser experiment.ml
$ ./parser "path-to-file-here.ml"
```
