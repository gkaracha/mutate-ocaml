
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

(* For now a rule is a function from an expression to an expression. *)
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
