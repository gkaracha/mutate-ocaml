type mutation_rule = Parsetree.expression -> Parsetree.expression option
let rule_true_false (expr : Parsetree.expression) =
  (let open Parsetree in
     match expr.pexp_desc with
     | Pexp_construct ({ txt = Longident.Lident "true"; loc }, None) ->
         Some
           {
             expr with
             pexp_desc =
               (Pexp_construct
                  ({ txt = (Longident.Lident "false"); loc }, None))
           }
     | _ -> None : Parsetree.expression option)[@@ocaml.doc
                                                 " Replace true by false. "]
let rule_false_true (expr : Parsetree.expression) =
  (let open Parsetree in
     match expr.pexp_desc with
     | Pexp_construct ({ txt = Longident.Lident "false"; loc }, None) ->
         Some
           {
             expr with
             pexp_desc =
               (Pexp_construct
                  ({ txt = (Longident.Lident "true"); loc }, None))
           }
     | _ -> None : Parsetree.expression option)[@@ocaml.doc
                                                 " Replace false by true. "]
let rule_and_or (expr : Parsetree.expression) =
  (let open Parsetree in
     match expr.pexp_desc with
     | Pexp_ident { txt = Longident.Lident "&&"; loc } ->
         Some
           {
             expr with
             pexp_desc = (Pexp_ident { txt = (Longident.Lident "||"); loc })
           }
     | _ -> None : Parsetree.expression option)[@@ocaml.doc
                                                 " Replace and (&&) by or (||). "]
let rule_or_and (expr : Parsetree.expression) =
  (let open Parsetree in
     match expr.pexp_desc with
     | Pexp_ident { txt = Longident.Lident "||"; loc } ->
         Some
           {
             expr with
             pexp_desc = (Pexp_ident { txt = (Longident.Lident "&&"); loc })
           }
     | _ -> None : Parsetree.expression option)[@@ocaml.doc
                                                 " Replace or (||) by and (&&). "]
let rule_append_swap_args (expr : Parsetree.expression) =
  (let open Parsetree in
     match expr.pexp_desc with
     | Pexp_apply (fn, (lbl1, e1)::(lbl2, e2)::[]) ->
         (match fn.pexp_desc with
          | Pexp_ident { txt = Longident.Lident "@"; loc = _loc } ->
              Some
                {
                  expr with
                  pexp_desc = (Pexp_apply (fn, [(lbl1, e2); (lbl2, e1)]))
                }
          | _ -> None)
     | _ -> None : Parsetree.expression option)[@@ocaml.doc
                                                 " Replace (e1 @ e2) by (e2 @ e1). "]
let rule_append_keep_first (expr : Parsetree.expression) =
  (let open Parsetree in
     match expr.pexp_desc with
     | Pexp_apply (fn, (_lbl1, e1)::_::[]) ->
         (match fn.pexp_desc with
          | Pexp_ident { txt = Longident.Lident "@"; loc = _loc } -> Some e1
          | _ -> None)
     | _ -> None : Parsetree.expression option)[@@ocaml.doc
                                                 " Replace (e1 @ e2) by (e1). "]
let rule_append_keep_second (expr : Parsetree.expression) =
  (let open Parsetree in
     match expr.pexp_desc with
     | Pexp_apply (fn, _::(_lbl2, e2)::[]) ->
         (match fn.pexp_desc with
          | Pexp_ident { txt = Longident.Lident "@"; loc = _loc } -> Some e2
          | _ -> None)
     | _ -> None : Parsetree.expression option)[@@ocaml.doc
                                                 " Replace (e1 @ e2) by (e2). "]
let rule_list_head (expr : Parsetree.expression) =
  (let open Parsetree in
     match expr.pexp_desc with
     | Pexp_construct ({ txt = Longident.Lident "::"; loc }, Some args) ->
         (match args.pexp_desc with
          | Pexp_tuple (e1::e2::[]) ->
              Some
                {
                  expr with
                  pexp_desc =
                    (Pexp_construct
                       ({ txt = (Longident.Lident "::"); loc },
                         (Some
                            {
                              args with
                              pexp_desc =
                                (Pexp_tuple
                                   [e1;
                                   {
                                     e2 with
                                     pexp_desc =
                                       (Pexp_construct
                                          ({
                                             txt = (Longident.Lident "[]");
                                             loc = (e2.pexp_loc)
                                           }, None))
                                   }])
                            })))
                }
          | _ -> None)
     | _ -> None : Parsetree.expression option)[@@ocaml.doc
                                                 " Replace (head :: tail) by ([head]). "]
let rule_list_tail (expr : Parsetree.expression) =
  (let open Parsetree in
     match expr.pexp_desc with
     | Pexp_construct
         ({ txt = Longident.Lident "::"; loc = _loc }, Some args) ->
         (match args.pexp_desc with
          | Pexp_tuple (_::e2::[]) -> Some e2
          | _ -> None)
     | _ -> None : Parsetree.expression option)[@@ocaml.doc
                                                 " Replace (head :: tail) by (tail). "]
