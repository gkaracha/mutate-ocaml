(** Parse an ocaml source file. *)
let parse_ml_file (src_file: string) : Parsetree.structure =
  Pparse.parse_implementation ~tool_name:"ocamlc" src_file

(** Print the AST in its raw form. *)
let print_raw_ast (ast: Parsetree.structure) : unit =
  Format.fprintf Format.std_formatter "%a@." Printast.implementation ast

(** Pretty-print the AST (parseable ocaml). *)
let print_ast (ast: Parsetree.structure) : unit =
  Format.fprintf Format.std_formatter "%a@." Pprintast.structure ast

let reformat_ast (src_file: string) : unit =
  let ast = parse_ml_file src_file in
  let oc = open_out src_file in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@." Pprintast.structure ast

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

let mapper_from_rules (rules: Rules.mutation_rule list) : (unit -> Ast_mapper.mapper) =
  fun () ->
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

let _ =
  (* just an example to see if it works OK. I've never used monads in OCaml before. *)
  let lst = Monad.run Monad.(
    (from_list [1; 2; 3]) >>= fun x ->
     return (10 * x) <|> return (10 * x + 1)
  ) in
  let () = List.iter (Printf.printf "%d ") lst in
  ()

(*
let () = Ast_mapper.register "ppx_test" (fun _ -> mapper_from_rules all_rules)
*)

(* (see Ast_mapper.apply_lazy in parsing/) *)
let apply_mapper mapper ast =
  let open Ast_mapper in
  let mapper = mapper () in
  mapper.structure mapper ast


let reformat_and_apply_rules_ast (src_file: string) : unit =
  let all_rules =
    [ Rules.rule_true_false
    ; Rules.rule_false_true
    ; Rules.rule_and_or
    ; Rules.rule_or_and
    ; Rules.rule_append_swap_args
    ; Rules.rule_append_keep_first
    ; Rules.rule_append_keep_second
    ; Rules.rule_list_head
    ; Rules.rule_list_tail
    ] in
  let ast = parse_ml_file src_file in
  let ast = apply_mapper (mapper_from_rules all_rules) ast in
  let oc = open_out src_file in
  let fmt = Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a@." Pprintast.structure ast


let example = true && false && (1 = 4)

let example2 = [1] @ []

(* More about ppx mappers here:
 * https://ocaml.org/api/compilerlibref/Ast_mapper.html *)
