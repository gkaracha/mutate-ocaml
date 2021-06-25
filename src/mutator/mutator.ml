
(** Parse an ocaml source file. *)
let parse_ml_file (src_file : string) : Parsetree.structure =
  Pparse.parse_implementation ~tool_name:"ocamlc" src_file

(** Print the AST in its raw form. *)
let print_raw_ast (ast: Parsetree.structure) : unit =
  Format.fprintf Format.std_formatter "%a@." Printast.implementation ast

(** Pretty-print the AST (parseable ocaml). *)
let print_ast (ast: Parsetree.structure) : unit =
  Format.fprintf Format.std_formatter "%a@." Pprintast.structure ast


(*
Mutations to consider
~~~~~~~~~~~~~~~~~~~~~
* Replace true by false
* Replace false by true

* Replace (head :: tail) by tail.
* Replace (head :: tail) by [head].
* Replace (list1 @ list2) by list1.
* Replace (list1 @ list2) by list2.
* Replace (list1 @ list2) by (list2 @ list1).

* Replace && by ||.
* Replace || by &&.

* Reorder pattern matching equations.
* Remove pattern matching equations.

Replace an integer constant N by one of {0, 1, -1, N+1, N-1}.
Replace an arithmetic / relational / logical operator by another in the same class.
Negate a conditional.

Type-aware
~~~~~~~~~~
* Replace a random list by the empty list [].

*)
