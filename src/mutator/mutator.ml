
(** Parse an ocaml source file. *)
let parse_ml_file (src_file : string) : Parsetree.structure =
  Pparse.parse_implementation ~tool_name:"ocamlc" src_file

(** Print the AST in its raw form. *)
let print_raw_ast (ast: Parsetree.structure) : unit =
  Format.fprintf Format.std_formatter "%a@." Printast.implementation ast

(** Pretty-print the AST (parseable ocaml). *)
let print_ast (ast: Parsetree.structure) : unit =
  Format.fprintf Format.std_formatter "%a@." Pprintast.structure ast

(* For now a rule is a function from an expression to an expression. *)
type mutation_rule = Parsetree.expression -> Parsetree.expression option

let rule_true_false expr =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_construct ({ txt = Longident.Lident "true";  loc = loc; }, None) ->
    Some { expr with pexp_desc = Pexp_construct ({ txt = Longident.Lident "false"; loc = loc; }, None) }
  | _ -> None

let rule_false_true expr =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_construct ({ txt = Longident.Lident "false";  loc = loc; }, None) ->
    Some { expr with pexp_desc = Pexp_construct ({ txt = Longident.Lident "true"; loc = loc; }, None) }
  | _ -> None

let rule_and_or expr =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_ident ({ txt = Longident.Lident "&&"; loc = loc; }) ->
    Some { expr with pexp_desc = Pexp_ident ({ txt = Longident.Lident "||"; loc = loc; }) }
  | _ -> None

let rule_or_and expr =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_ident ({ txt = Longident.Lident "||"; loc = loc; }) ->
    Some { expr with pexp_desc = Pexp_ident ({ txt = Longident.Lident "&&"; loc = loc; }) }
  | _ -> None


(* TODO: add more rules here
 * - Replace an arithmetic / relational / logical operator by another in the same class.
 * - Replace (head :: tail) by tail.
 * - Replace (head :: tail) by [head].
 * - Replace (list1 @ list2) by list1.
 * - Replace (list1 @ list2) by list2.
 * - Replace (list1 @ list2) by (list2 @ list1).
 * - Replace an integer constant N by one of {0, 1, -1, N+1, N-1}.
 * - Negate a conditional.
*)
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
    replacement_rules : mutation_rule list;
  }

(** Default mutation configuration: do nothing. *)
let default_configuration =
  { negate_conditionals = false;
    reorder_match_clauses = false;
    remove_match_clauses = false;
    replacement_rules = [];
  }

let mapper_from_rules rules =
  Ast_mapper.{
    default_mapper with
    expr = fun mapper expr ->
      let rec try_sequentially rs =
        match rs with
        | [] -> default_mapper.expr mapper expr
        | r :: rs -> (
            match r expr with
            | Some expr -> expr
            | None -> try_sequentially rs
          )
      in try_sequentially rules
  }

let example = true && false && (1 = 4)

(* More about ppx mappers here:
 * https://ocaml.org/api/compilerlibref/Ast_mapper.html *)
