
(** Parse an ocaml source file. *)
let parse_ml_file (src_file : string) : Parsetree.structure =
  Pparse.parse_implementation ~tool_name:"ocamlc" src_file

(** Print the AST in its raw form. *)
let print_raw_ast (ast: Parsetree.structure) : unit =
  Format.fprintf Format.std_formatter "%a@." Printast.implementation ast

(** Pretty-print the AST (parseable ocaml). *)
let print_ast (ast: Parsetree.structure) : unit =
  Format.fprintf Format.std_formatter "%a@." Pprintast.structure ast

(** Replacement rules for closed expressions. *)
type expression_replacement_rule =
  { from_expr: Parsetree.expression_desc; to_expr: Parsetree.expression_desc; }

let rule_true_false =
  {
    from_expr = Parsetree.Pexp_construct (Location.mknoloc (Longident.Lident "true" ), None);
    to_expr   = Parsetree.Pexp_construct (Location.mknoloc (Longident.Lident "false"), None);
  }

let rule_false_true =
  {
    from_expr = Parsetree.Pexp_construct (Location.mknoloc (Longident.Lident "false"), None);
    to_expr   = Parsetree.Pexp_construct (Location.mknoloc (Longident.Lident "true" ), None);
  }

let rule_and_or =
  {
    from_expr = Parsetree.Pexp_ident (Location.mknoloc (Longident.Lident "&&"));
    to_expr   = Parsetree.Pexp_ident (Location.mknoloc (Longident.Lident "||"));
  }

let rule_or_and =
  {
    from_expr = Parsetree.Pexp_ident (Location.mknoloc (Longident.Lident "||"));
    to_expr   = Parsetree.Pexp_ident (Location.mknoloc (Longident.Lident "&&"));
  }

(* TODO: add more rules here (Replace an arithmetic / relational / logical
 * operator by another in the same class.) *)

(* Template matching *)
(* TODO: Figure out how to represent template matching nicely. Perhaps via
 * functions
 *
 * - Replace (head :: tail) by tail.
 * - Replace (head :: tail) by [head].
 * - Replace (list1 @ list2) by list1.
 * - Replace (list1 @ list2) by list2.
 * - Replace (list1 @ list2) by (list2 @ list1).
 * - Replace an integer constant N by one of {0, 1, -1, N+1, N-1}.
 * - Negate a conditional.
*)
(* TODO: Perhaps have only two kinds of rules: matching and equality. The
 * former have holes in them, while the second are closed expressions. *)
(* TODO: Type-aware: replace a random list by the empty list. *)

(* TODO: Should populate this from a JSON input or something. *)
type mutation_configuration =
  {
    (* Whether to negate the condition in if-then-else expressions. *)
    negate_conditionals : bool;

    (* Whether to reorder clauses in pattern matching expressions. *)
    reorder_match_clauses : bool;

    (* Whether to remove clauses in pattern matching expressions. *)
    remove_match_clauses : bool;

    (* Rules for replacing closed expressions by other closed expressions. *)
    replacement_rules : expression_replacement_rule list;
  }

(** Default mutation configuration: do nothing. *)
let default_configuration =
  { negate_conditionals = false;
    reorder_match_clauses = false;
    remove_match_clauses = false;
    replacement_rules = [];
  }
