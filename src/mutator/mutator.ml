(** Parse an ocaml source file. *)
let parse_ml_file (src_file : string) : Parsetree.structure =
  Pparse.parse_implementation ~tool_name:"ocamlc" src_file

(** Print the AST in its raw form. *)
let print_raw_ast (ast: Parsetree.structure) : unit =
  Format.fprintf Format.std_formatter "%a@." Printast.implementation ast

(** Pretty-print the AST (parseable ocaml). *)
let print_ast (ast: Parsetree.structure) : unit =
  Format.fprintf Format.std_formatter "%a@." Pprintast.structure ast

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
    replacement_rules : Rules.mutation_rule list;
  }

(** Default mutation configuration: do nothing. *)
let default_configuration =
  { negate_conditionals = false;
    reorder_match_clauses = false;
    remove_match_clauses = false;
    replacement_rules = [];
  }

let mapper_from_rules (rules: Rules.mutation_rule list) : Ast_mapper.mapper =
  Ast_mapper.{
    default_mapper with
    expr = fun mapper expr ->
      let rec try_sequentially rs =
        match rs with
        | [] -> default_mapper.expr mapper expr
        | r :: rs ->
          match r expr with
          | Some expr -> expr
          | None -> try_sequentially rs
      in try_sequentially rules
  }

(*
let () = Ast_mapper.register "ppx_test" (fun _ -> mapper_from_rules all_rules)
*)

let example = true && false && (1 = 4)

let example2 = [1] @ []

(* More about ppx mappers here:
 * https://ocaml.org/api/compilerlibref/Ast_mapper.html *)
