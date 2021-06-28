
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
type mutation_rule = (Parsetree.expression -> Parsetree.expression option)

(** Replace true by false. *)
let rule_true_false (expr: Parsetree.expression) : Parsetree.expression option =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_construct ({ txt = Longident.Lident "true";  loc = loc; }, None) ->
    Some { expr with pexp_desc = Pexp_construct ({ txt = Longident.Lident "false"; loc = loc; }, None) }
  | _ -> None

(** Replace false by true. *)
let rule_false_true (expr: Parsetree.expression) : Parsetree.expression option =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_construct ({ txt = Longident.Lident "false";  loc = loc; }, None) ->
    Some { expr with pexp_desc = Pexp_construct ({ txt = Longident.Lident "true"; loc = loc; }, None) }
  | _ -> None

(** Replace and (&&) by or (||). *)
let rule_and_or (expr: Parsetree.expression) : Parsetree.expression option =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_ident ({ txt = Longident.Lident "&&"; loc = loc; }) ->
    Some { expr with pexp_desc = Pexp_ident ({ txt = Longident.Lident "||"; loc = loc; }) }
  | _ -> None

(** Replace or (||) by and (&&). *)
let rule_or_and (expr: Parsetree.expression) : Parsetree.expression option =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_ident ({ txt = Longident.Lident "||"; loc = loc; }) ->
    Some { expr with pexp_desc = Pexp_ident ({ txt = Longident.Lident "&&"; loc = loc; }) }
  | _ -> None

(** Replace (e1 @ e2) by (e2 @ e1). *)
let rule_append_swap_args (expr: Parsetree.expression) : Parsetree.expression option =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_apply (fn, [(lbl1, e1); (lbl2, e2)]) ->
     begin match fn.pexp_desc with
     | Pexp_ident ({ txt = Longident.Lident "@"; loc = _loc; }) ->
       Some { expr with pexp_desc = Pexp_apply (fn, [(lbl1, e2); (lbl2, e1)]) }
     | _ -> None
     end
  | _ -> None

(** Replace (e1 @ e2) by (e1). *)
let rule_append_keep_first (expr: Parsetree.expression) : Parsetree.expression option =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_apply (fn, [(_lbl1, e1); _]) ->
     begin match fn.pexp_desc with
     | Pexp_ident ({ txt = Longident.Lident "@"; loc = _loc; }) -> Some e1
     | _ -> None
     end
  | _ -> None

(** Replace (e1 @ e2) by (e2). *)
let rule_append_keep_second (expr: Parsetree.expression) : Parsetree.expression option =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_apply (fn, [_; (_lbl2, e2)]) ->
     begin match fn.pexp_desc with
     | Pexp_ident ({ txt = Longident.Lident "@"; loc = _loc; }) -> Some e2
     | _ -> None
     end
  | _ -> None

(** Replace (head :: tail) by ([head]). *)
let rule_list_head (expr: Parsetree.expression) : Parsetree.expression option =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_construct ({ txt = Longident.Lident "::"; loc = loc; }, Some args) ->
    begin match args.pexp_desc with
    | Pexp_tuple ([e1; e2]) ->
      Some
        { expr with pexp_desc =
            Pexp_construct
              ( { txt = Longident.Lident "::"; loc = loc; },
                Some
                  { args with pexp_desc =
                      Pexp_tuple ([e1; { e2 with pexp_desc = Pexp_construct ({ txt = Longident.Lident "true"; loc = e2.pexp_loc; }, None) } ])
                  }
              )
        }
    | _ -> None
    end
  | _ -> None

(** Replace (head :: tail) by (tail). *)
let rule_list_tail (expr: Parsetree.expression) : Parsetree.expression option =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_construct ({ txt = Longident.Lident "::"; loc = _loc; }, Some args) ->
    begin match args.pexp_desc with
    | Pexp_tuple ([_; e2]) -> Some e2
    | _ -> None
    end
  | _ -> None



(* TODO: add more rules here
 * - Replace an arithmetic / relational / logical operator by another in the same class.
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
        | r :: rs ->
          match r expr with
          | Some expr -> expr
          | None -> try_sequentially rs
      in try_sequentially rules
  }

let all_rules =
  [
    rule_true_false;
    rule_false_true;
    rule_and_or;
    rule_or_and;
  ]

(*
let () = Ast_mapper.register "ppx_test" (fun _ -> mapper_from_rules all_rules)
*)

let example = true && false && (1 = 4)

let example2 = [1] @ []

(* More about ppx mappers here:
 * https://ocaml.org/api/compilerlibref/Ast_mapper.html *)
