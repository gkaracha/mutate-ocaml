(* Mutator *)

open Parsetree
open Ast_helper
open Location

(* TODO: I would have preferred to be able to recurse over every bit of data. *)
type mapper = {
  attribute: mapper -> attribute -> attribute Sequence.t;
  attributes: mapper -> attribute list -> (attribute list) Sequence.t;
  binding_op: mapper -> binding_op -> binding_op Sequence.t;
  case: mapper -> case -> case Sequence.t;
  cases: mapper -> case list -> (case list) Sequence.t;
  class_declaration: mapper -> class_declaration -> class_declaration Sequence.t;
  class_description: mapper -> class_description -> class_description Sequence.t;
  class_expr: mapper -> class_expr -> class_expr Sequence.t;
  class_field: mapper -> class_field -> class_field Sequence.t;
  class_signature: mapper -> class_signature -> class_signature Sequence.t;
  class_structure: mapper -> class_structure -> class_structure Sequence.t;
  class_type: mapper -> class_type -> class_type Sequence.t;
  class_type_declaration: mapper -> class_type_declaration -> class_type_declaration Sequence.t;
  class_type_field: mapper -> class_type_field -> class_type_field Sequence.t;
  constant: mapper -> constant -> constant Sequence.t;
  constructor_declaration: mapper -> constructor_declaration -> constructor_declaration Sequence.t;
  expr: mapper -> expression -> expression Sequence.t;
  extension: mapper -> extension -> extension Sequence.t;
  extension_constructor: mapper -> extension_constructor -> extension_constructor Sequence.t;
  include_declaration: mapper -> include_declaration -> include_declaration Sequence.t;
  include_description: mapper -> include_description -> include_description Sequence.t;
  label_declaration: mapper -> label_declaration -> label_declaration Sequence.t;
  location: mapper -> Location.t -> Location.t Sequence.t;
  module_binding: mapper -> module_binding -> module_binding Sequence.t;
  module_declaration: mapper -> module_declaration -> module_declaration Sequence.t;
  module_substitution: mapper -> module_substitution -> module_substitution Sequence.t;
  module_expr: mapper -> module_expr -> module_expr Sequence.t;
  module_type: mapper -> module_type -> module_type Sequence.t;
  module_type_declaration: mapper -> module_type_declaration -> module_type_declaration Sequence.t;
  open_declaration: mapper -> open_declaration -> open_declaration Sequence.t;
  open_description: mapper -> open_description -> open_description Sequence.t;
  pat: mapper -> pattern -> pattern Sequence.t;
  payload: mapper -> payload -> payload Sequence.t;
  signature: mapper -> signature -> signature Sequence.t;
  signature_item: mapper -> signature_item -> signature_item Sequence.t;
  structure: mapper -> structure -> structure Sequence.t;
  structure_item: mapper -> structure_item -> structure_item Sequence.t;
  typ: mapper -> core_type -> core_type Sequence.t;
  type_declaration: mapper -> type_declaration -> type_declaration Sequence.t;
  type_extension: mapper -> type_extension -> type_extension Sequence.t;
  type_exception: mapper -> type_exception -> type_exception Sequence.t;
  type_kind: mapper -> type_kind -> type_kind Sequence.t;
  value_binding: mapper -> value_binding -> value_binding Sequence.t;
  value_description: mapper -> value_description -> value_description Sequence.t;
  with_constraint: mapper -> with_constraint -> with_constraint Sequence.t;
}

(* Some utilities *)
let map_fst f (x, y) = Sequence.fmap (fun z -> (z, y)) (f x)
let map_snd f (x, y) = Sequence.fmap (fun z -> (z, y)) (f x)

let map_tuple f1 f2 (x, y) =
  let x_mutants = Sequence.fmap (fun x -> (x, y)) (f1 x) in
  let y_mutants = Sequence.fmap (fun y -> (x, y)) (f2 y) in
  Sequence.(x_mutants $$ y_mutants)

let map_tuple3 f1 f2 f3 (x, y, z) =
  let x_mutants = Sequence.fmap (fun x -> (x, y, z)) (f1 x) in
  let y_mutants = Sequence.fmap (fun y -> (x, y, z)) (f2 y) in
  let z_mutants = Sequence.fmap (fun z -> (x, y, z)) (f3 z) in
  Sequence.(x_mutants $$ y_mutants $$ z_mutants)

let map_opt f = function
  | None -> Sequence.empty
  | Some x -> Sequence.fmap (fun x -> Some x) (f x)

let rec map_list f = function
  | [] -> Sequence.empty
  | x :: xs ->
    let x_mutants = Sequence.fmap (fun x -> (x :: xs)) (f x) in
    let xs_mutants = Sequence.fmap (fun xs -> (x :: xs)) (map_list f xs) in
    Sequence.(x_mutants $$ xs_mutants)

let map_loc sub {loc; txt} =
  Sequence.fmap
    (fun loc -> {loc = loc; txt})
    (sub.location sub loc)

module C = struct
  (* Constants *)
  let map sub c =
    match c with
    | Pconst_integer _
    | Pconst_char _
    | Pconst_float _
      -> Sequence.empty (* TODO: Hmmm, how come we don't do anything to these constants? *)
    | Pconst_string (s, loc, quotation_delimiter) ->
      Sequence.fmap
        (fun loc -> Const.string ~loc ?quotation_delimiter s)
        (sub.location sub loc)
end

module T = struct
  (* Type expressions for the core language *)

  let row_field sub {
      prf_desc;
      prf_loc;
      prf_attributes;
    } =
    let loc_mutants =
      Sequence.fmap
        (fun prf_loc -> Rf.mk ~loc:prf_loc ~attrs:prf_attributes prf_desc)
        (sub.location sub prf_loc) in
    let attr_mutants =
      Sequence.fmap
        (fun prf_attributes -> Rf.mk ~loc:prf_loc ~attrs:prf_attributes prf_desc)
        (sub.attributes sub prf_attributes) in
    let desc_mutants =
      Sequence.fmap
        (fun prf_desc -> Rf.mk ~loc:prf_loc ~attrs:prf_attributes prf_desc)
        (match prf_desc with
         | Rtag (l, b, tl) ->
           let l_mutants  = Sequence.fmap (fun l -> Rtag (l, b, tl)) (map_loc sub l) in
           let tl_mutants = Sequence.fmap (fun tl -> Rtag (l, b, tl)) (map_list (sub.typ sub) tl) in
           Sequence.(l_mutants $$ tl_mutants)
         | Rinherit t ->
           Sequence.fmap (fun t -> Rinherit t) (sub.typ sub t)
        ) in
    Sequence.(loc_mutants $$ attr_mutants $$ desc_mutants)

  let object_field sub {
      pof_desc;
      pof_loc;
      pof_attributes;
    } =
    let loc_mutants =
      Sequence.fmap
        (fun pof_loc -> Of.mk ~loc:pof_loc ~attrs:pof_attributes pof_desc)
        (sub.location sub pof_loc) in
    let attr_mutants =
      Sequence.fmap
        (fun pof_attributes -> Of.mk ~loc:pof_loc ~attrs:pof_attributes pof_desc)
        (sub.attributes sub pof_attributes) in
    let desc_mutants =
      Sequence.fmap
        (fun pof_desc -> Of.mk ~loc:pof_loc ~attrs:pof_attributes pof_desc)
        (match pof_desc with
         | Otag (l, t) ->
           let l_mutants = Sequence.fmap (fun l -> Otag (l, t)) (map_loc sub l) in
           let t_mutants = Sequence.fmap (fun t -> Otag (l, t)) (sub.typ sub t) in
           Sequence.(l_mutants $$ t_mutants)
         | Oinherit t ->
           Sequence.fmap (fun t -> Oinherit t) (sub.typ sub t)
        ) in
    Sequence.(loc_mutants $$ attr_mutants $$ desc_mutants)

  let map sub {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs; ptyp_loc_stack; } =
    let loc_mutants =
      Sequence.fmap
        (fun loc -> {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs; ptyp_loc_stack; })
        (sub.location sub loc) in
    let attr_mutants =
      Sequence.fmap
        (fun attrs -> {ptyp_desc = desc; ptyp_loc = loc; ptyp_attributes = attrs; ptyp_loc_stack; })
        (sub.attributes sub attrs) in
    let desc_mutants =
      match desc with
      | Ptyp_any -> Sequence.empty
      | Ptyp_var _s -> Sequence.empty
      | Ptyp_arrow (lab, t1, t2) ->
          let t1_mutants = Sequence.fmap (fun t1 -> Typ.arrow ~loc ~attrs lab t1 t2) (sub.typ sub t1) in
          let t2_mutants = Sequence.fmap (fun t2 -> Typ.arrow ~loc ~attrs lab t1 t2) (sub.typ sub t2) in
          Sequence.(t1_mutants $$ t2_mutants)
      | Ptyp_tuple tyl ->
          Sequence.fmap (fun tyl -> Typ.tuple ~loc ~attrs tyl) (map_list (sub.typ sub) tyl)
      | Ptyp_constr (lid, tl) ->
          let lid_mutants = Sequence.fmap (fun lid -> Typ.constr ~loc ~attrs lid tl) (map_loc sub lid) in
          let tl_mutants = Sequence.fmap (fun tl -> Typ.constr ~loc ~attrs lid tl) (map_list (sub.typ sub) tl) in
          Sequence.(lid_mutants $$ tl_mutants)
      | Ptyp_object (l, o) ->
          Sequence.fmap (fun l -> Typ.object_ ~loc ~attrs l o) (map_list (object_field sub) l)
      | Ptyp_class (lid, tl) ->
          let lid_mutants = Sequence.fmap (fun lid -> Typ.class_ ~loc ~attrs lid tl) (map_loc sub lid) in
          let tl_mutants = Sequence.fmap (fun tl -> Typ.class_ ~loc ~attrs lid tl) (map_list (sub.typ sub) tl) in
          Sequence.(lid_mutants $$ tl_mutants)
      | Ptyp_alias (t, s) ->
          Sequence.fmap (fun t -> Typ.alias ~loc ~attrs t s) (sub.typ sub t)
      | Ptyp_variant (rl, b, ll) ->
          Sequence.fmap (fun rl -> Typ.variant ~loc ~attrs rl b ll) (map_list (row_field sub) rl)
      | Ptyp_poly (sl, t) ->
          let sl_mutants = Sequence.fmap (fun sl -> Typ.poly ~loc ~attrs sl t) (map_list (map_loc sub) sl) in
          let t_mutants = Sequence.fmap (fun t -> Typ.poly ~loc ~attrs sl t) (sub.typ sub t) in
          Sequence.(sl_mutants $$ t_mutants)
      | Ptyp_package (lid, l) ->
          let lid_mutants = Sequence.fmap (fun lid -> Typ.package ~loc ~attrs lid l) (map_loc sub lid) in
          let l_mutants = Sequence.fmap (fun l -> Typ.package ~loc ~attrs lid l) (map_list (map_tuple (map_loc sub) (sub.typ sub)) l) in
          Sequence.(lid_mutants $$ l_mutants)
      | Ptyp_extension x ->
          Sequence.fmap (fun x -> Typ.extension ~loc ~attrs x) (sub.extension sub x)
    in
    Sequence.(loc_mutants $$ attr_mutants $$ desc_mutants)

(*
  let map_type_declaration sub
      {ptype_name; ptype_params; ptype_cstrs;
       ptype_kind;
       ptype_private;
       ptype_manifest;
       ptype_attributes;
       ptype_loc} =
    let open Monad in
    sub.location sub ptype_loc                >>= fun loc ->
    sub.attributes sub ptype_attributes       >>= fun attrs ->
    map_loc sub ptype_name                    >>= fun ptype_name ->
    mmap (map_fst (sub.typ sub)) ptype_params >>= fun ptype_params ->
    mmap (map_tuple3 (sub.typ sub) (sub.typ sub) (sub.location sub)) ptype_cstrs >>= fun ptype_cstrs ->
    sub.type_kind sub ptype_kind              >>= fun ptype_kind ->
    map_opt (sub.typ sub) ptype_manifest      >>= fun ptype_manifest ->
    return (
      Type.mk ~loc ~attrs ptype_name
        ~params:ptype_params
        ~priv:ptype_private
        ~cstrs:ptype_cstrs
        ~kind:ptype_kind
        ?manifest:ptype_manifest
    )

  let map_type_kind sub =
    let open Monad in
    function
    | Ptype_abstract -> return Ptype_abstract
    | Ptype_variant l ->
        mmap (sub.constructor_declaration sub) l >>= fun l ->
        return (Ptype_variant l)
    | Ptype_record l ->
        mmap (sub.label_declaration sub) l >>= fun l ->
        return (Ptype_record l)
    | Ptype_open -> return Ptype_open

  let map_constructor_arguments sub =
    let open Monad in
    function
    | Pcstr_tuple l ->
      mmap (sub.typ sub) l >>= fun l ->
      return (Pcstr_tuple l)
    | Pcstr_record l ->
        mmap (sub.label_declaration sub) l >>= fun l ->
        return (Pcstr_record l)

  let map_type_extension sub
      {ptyext_path; ptyext_params;
       ptyext_constructors;
       ptyext_private;
       ptyext_loc;
       ptyext_attributes} =
    let open Monad in
    sub.location sub ptyext_loc                              >>= fun loc ->
    sub.attributes sub ptyext_attributes                     >>= fun attrs ->
    map_loc sub ptyext_path                                  >>= fun ptyext_path ->
    mmap (sub.extension_constructor sub) ptyext_constructors >>= fun ptyext_constructors ->
    mmap (map_fst (sub.typ sub)) ptyext_params               >>= fun ptyext_params ->
    return (
      Te.mk ~loc ~attrs
        ptyext_path
        ptyext_constructors
        ~params:ptyext_params
        ~priv:ptyext_private
    )

  let map_type_exception sub
      {ptyexn_constructor; ptyexn_loc; ptyexn_attributes} =
    let open Monad in
    sub.location sub ptyexn_loc                      >>= fun loc ->
    sub.attributes sub ptyexn_attributes             >>= fun attrs ->
    sub.extension_constructor sub ptyexn_constructor >>= fun ptyexn_constructor ->
    return (Te.mk_exception ~loc ~attrs ptyexn_constructor)

  let map_extension_constructor_kind sub =
    let open Monad in
    function
    | Pext_decl(ctl, cto) ->
      map_constructor_arguments sub ctl >>= fun ctl ->
      map_opt (sub.typ sub) cto         >>= fun cto ->
      return (Pext_decl (ctl, cto))
    | Pext_rebind li ->
      map_loc sub li >>= fun li ->
      return (Pext_rebind li)

  let map_extension_constructor sub
      {pext_name;
       pext_kind;
       pext_loc;
       pext_attributes} =
    let open Monad in
    sub.location sub pext_loc          >>= fun loc ->
    sub.attributes sub pext_attributes >>= fun attrs ->
    Te.constructor ~loc ~attrs
      <$> (map_loc sub pext_name)
      <*> (map_extension_constructor_kind sub pext_kind)
*)
end

(*
module CT = struct
  (* Type expressions for the class language *)

  let map sub {pcty_loc = loc; pcty_desc = desc; pcty_attributes = attrs} =
    let open Cty in
    let open Monad in
    sub.location sub loc     >>= fun loc ->
    sub.attributes sub attrs >>= fun attrs ->
    match desc with
    | Pcty_constr (lid, tys) ->
        constr ~loc ~attrs <$> (map_loc sub lid) <*> (mmap (sub.typ sub) tys)
    | Pcty_signature x -> signature ~loc ~attrs <$> (sub.class_signature sub x)
    | Pcty_arrow (lab, t, ct) ->
        arrow ~loc ~attrs lab <$> (sub.typ sub t) <*> (sub.class_type sub ct)
    | Pcty_extension x -> extension ~loc ~attrs <$> (sub.extension sub x)
    | Pcty_open (o, ct) ->
        open_ ~loc ~attrs <$> (sub.open_description sub o) <*> (sub.class_type sub ct)

  let map_field sub {pctf_desc = desc; pctf_loc = loc; pctf_attributes = attrs} =
    let open Ctf in
    let open Monad in
    sub.location sub loc     >>= fun loc ->
    sub.attributes sub attrs >>= fun attrs ->
    match desc with
    | Pctf_inherit ct -> inherit_ ~loc ~attrs <$> (sub.class_type sub ct)
    | Pctf_val (s, m, v, t) ->
        val_ ~loc ~attrs <$> (map_loc sub s) <*> (return m) <*> (return v) <*> (sub.typ sub t)
    | Pctf_method (s, p, v, t) ->
        method_ ~loc ~attrs <$> (map_loc sub s) <*> (return p) <*> (return v) <*> (sub.typ sub t)
    | Pctf_constraint (t1, t2) ->
        constraint_ ~loc ~attrs <$> (sub.typ sub t1) <*> (sub.typ sub t2)
    | Pctf_attribute x -> attribute ~loc <$> (sub.attribute sub x)
    | Pctf_extension x -> extension ~loc ~attrs <$> (sub.extension sub x)

  let map_signature sub {pcsig_self; pcsig_fields} =
    let open Monad in
    Csig.mk
      <$> (sub.typ sub pcsig_self)
      <*> (mmap (sub.class_type_field sub) pcsig_fields)
end

let map_functor_param sub =
  let open Monad in
  function
  | Unit -> return Unit
  | Named (s, mt) ->
    map_loc sub s          >>= fun s ->
    sub.module_type sub mt >>= fun mt ->
    return (Named (s, mt))

module MT = struct
  (* Type expressions for the module language *)

  let map sub {pmty_desc = desc; pmty_loc = loc; pmty_attributes = attrs} =
    let open Mty in
    let open Monad in
    sub.location sub loc     >>= fun loc ->
    sub.attributes sub attrs >>= fun attrs ->
    match desc with
    | Pmty_ident s -> ident ~loc ~attrs <$> (map_loc sub s)
    | Pmty_alias s -> alias ~loc ~attrs <$> (map_loc sub s)
    | Pmty_signature sg -> signature ~loc ~attrs <$> (sub.signature sub sg)
    | Pmty_functor (param, mt) ->
        functor_ ~loc ~attrs
          <$> (map_functor_param sub param)
          <*> (sub.module_type sub mt)
    | Pmty_with (mt, l) ->
        with_ ~loc ~attrs
          <$> (sub.module_type sub mt)
          <*> (mmap (sub.with_constraint sub) l)
    | Pmty_typeof me -> typeof_ ~loc ~attrs <$> (sub.module_expr sub me)
    | Pmty_extension x -> extension ~loc ~attrs <$> (sub.extension sub x)

  let map_with_constraint sub =
    let open Monad in
    function
    | Pwith_type (lid, d) ->
        map_loc sub lid            >>= fun lid ->
        sub.type_declaration sub d >>= fun d ->
        return (Pwith_type (lid, d))
    | Pwith_module (lid, lid2) ->
        map_loc sub lid  >>= fun lid ->
        map_loc sub lid2 >>= fun lid2 ->
        return (Pwith_module (lid, lid2))
    | Pwith_typesubst (lid, d) ->
        map_loc sub lid            >>= fun lid ->
        sub.type_declaration sub d >>= fun d ->
        return (Pwith_typesubst (lid, d))
    | Pwith_modsubst (s, lid) ->
        map_loc sub s   >>= fun s ->
        map_loc sub lid >>= fun lid ->
        return (Pwith_modsubst (s, lid))

  let map_signature_item sub {psig_desc = desc; psig_loc = loc} =
    let open Sig in
    let open Monad in
    sub.location sub loc >>= fun loc ->
    match desc with
    | Psig_value vd -> value ~loc <$> (sub.value_description sub vd)
    | Psig_type (rf, l) -> type_ ~loc rf <$> (mmap (sub.type_declaration sub) l)
    | Psig_typesubst l -> type_subst ~loc <$> (mmap (sub.type_declaration sub) l)
    | Psig_typext te -> type_extension ~loc <$> (sub.type_extension sub te)
    | Psig_exception ed -> exception_ ~loc <$> (sub.type_exception sub ed)
    | Psig_module x -> module_ ~loc <$> (sub.module_declaration sub x)
    | Psig_modsubst x -> mod_subst ~loc <$> (sub.module_substitution sub x)
    | Psig_recmodule l -> rec_module ~loc <$> (mmap (sub.module_declaration sub) l)
    | Psig_modtype x -> modtype ~loc <$> (sub.module_type_declaration sub x)
    | Psig_open x -> open_ ~loc <$> (sub.open_description sub x)
    | Psig_include x -> include_ ~loc <$> (sub.include_description sub x)
    | Psig_class l -> class_ ~loc <$> (mmap (sub.class_description sub) l)
    | Psig_class_type l -> class_type ~loc <$> (mmap (sub.class_type_declaration sub) l)
    | Psig_extension (x, attrs) ->
        sub.attributes sub attrs >>= fun attrs ->
        extension ~loc ~attrs <$> (sub.extension sub x)
    | Psig_attribute x -> attribute ~loc <$> (sub.attribute sub x)
end

module M = struct
  (* Value expressions for the module language *)

  let map sub {pmod_loc = loc; pmod_desc = desc; pmod_attributes = attrs} =
    let open Mod in
    let open Monad in
    sub.location sub loc     >>= fun loc ->
    sub.attributes sub attrs >>= fun attrs ->
    match desc with
    | Pmod_ident x -> ident ~loc ~attrs <$> (map_loc sub x)
    | Pmod_structure str -> structure ~loc ~attrs <$> (sub.structure sub str)
    | Pmod_functor (param, body) ->
        functor_ ~loc ~attrs
          <$> (map_functor_param sub param)
          <*> (sub.module_expr sub body)
    | Pmod_apply (m1, m2) ->
        apply ~loc ~attrs <$> (sub.module_expr sub m1) <*> (sub.module_expr sub m2)
    | Pmod_constraint (m, mty) ->
        constraint_ ~loc ~attrs
          <$> (sub.module_expr sub m)
          <*> (sub.module_type sub mty)
    | Pmod_unpack e -> unpack ~loc ~attrs <$> (sub.expr sub e)
    | Pmod_extension x -> extension ~loc ~attrs <$> (sub.extension sub x)

  let map_structure_item sub {pstr_loc = loc; pstr_desc = desc} =
    let open Str in
    let open Monad in
    sub.location sub loc >>= fun loc ->
    match desc with
    | Pstr_eval (x, attrs) ->
        sub.attributes sub attrs >>= fun attrs ->
        eval ~loc ~attrs <$> (sub.expr sub x)
    | Pstr_value (r, vbs) -> value ~loc r <$> (mmap (sub.value_binding sub) vbs)
    | Pstr_primitive vd -> primitive ~loc <$> (sub.value_description sub vd)
    | Pstr_type (rf, l) -> type_ ~loc rf <$> (mmap (sub.type_declaration sub) l)
    | Pstr_typext te -> type_extension ~loc <$> (sub.type_extension sub te)
    | Pstr_exception ed -> exception_ ~loc <$> (sub.type_exception sub ed)
    | Pstr_module x -> module_ ~loc <$> (sub.module_binding sub x)
    | Pstr_recmodule l -> rec_module ~loc <$> (mmap (sub.module_binding sub) l)
    | Pstr_modtype x -> modtype ~loc <$> (sub.module_type_declaration sub x)
    | Pstr_open x -> open_ ~loc <$> (sub.open_declaration sub x)
    | Pstr_class l -> class_ ~loc <$> (mmap (sub.class_declaration sub) l)
    | Pstr_class_type l -> class_type ~loc <$> (mmap (sub.class_type_declaration sub) l)
    | Pstr_include x -> include_ ~loc <$> (sub.include_declaration sub x)
    | Pstr_extension (x, attrs) ->
        sub.attributes sub attrs >>= fun attrs ->
        extension ~loc ~attrs <$> (sub.extension sub x)
    | Pstr_attribute x -> attribute ~loc <$> (sub.attribute sub x)
end

module E = struct
  (* Value expressions for the core language *)

  let map sub {pexp_loc = loc; pexp_desc = desc; pexp_attributes = attrs; pexp_loc_stack = _} =
    let open Exp in
    let open Monad in
    sub.location sub loc     >>= fun loc ->
    sub.attributes sub attrs >>= fun attrs ->
    match desc with
    | Pexp_ident x -> ident ~loc ~attrs <$> (map_loc sub x)
    | Pexp_constant x -> constant ~loc ~attrs <$> (sub.constant sub x)
    | Pexp_let (r, vbs, e) ->
        let_ ~loc ~attrs r
          <$> (mmap (sub.value_binding sub) vbs)
          <*> (sub.expr sub e)
    | Pexp_fun (lab, def, p, e) ->
        fun_ ~loc ~attrs lab
          <$> (map_opt (sub.expr sub) def)
          <*> (sub.pat sub p)
          <*> (sub.expr sub e)
    | Pexp_function pel -> function_ ~loc ~attrs <$> (sub.cases sub pel)
    | Pexp_apply (e, l) -> apply ~loc ~attrs <$> (sub.expr sub e) <*> (mmap (map_snd (sub.expr sub)) l)
    | Pexp_match (e, pel) -> match_ ~loc ~attrs <$> (sub.expr sub e) <*> (sub.cases sub pel)
    | Pexp_try (e, pel) -> try_ ~loc ~attrs <$> (sub.expr sub e) <*> (sub.cases sub pel)
    | Pexp_tuple el -> tuple ~loc ~attrs <$> (mmap (sub.expr sub) el)
    | Pexp_construct (lid, arg) -> construct ~loc ~attrs <$> (map_loc sub lid) <*> (map_opt (sub.expr sub) arg)
    | Pexp_variant (lab, eo) -> variant ~loc ~attrs lab <$> (map_opt (sub.expr sub) eo)
    | Pexp_record (l, eo) ->
        record ~loc ~attrs
          <$> (mmap (map_tuple (map_loc sub) (sub.expr sub)) l)
          <*> (map_opt (sub.expr sub) eo)
    | Pexp_field (e, lid) -> field ~loc ~attrs <$> (sub.expr sub e) <*> (map_loc sub lid)
    | Pexp_setfield (e1, lid, e2) ->
        setfield ~loc ~attrs
          <$> (sub.expr sub e1)
          <*> (map_loc sub lid)
          <*> (sub.expr sub e2)
    | Pexp_array el -> array ~loc ~attrs <$> (mmap (sub.expr sub) el)
    | Pexp_ifthenelse (e1, e2, e3) ->
        ifthenelse ~loc ~attrs
          <$> (sub.expr sub e1)
          <*> (sub.expr sub e2)
          <*> (map_opt (sub.expr sub) e3)
    | Pexp_sequence (e1, e2) ->
        sequence ~loc ~attrs <$> (sub.expr sub e1) <*> (sub.expr sub e2)
    | Pexp_while (e1, e2) ->
        while_ ~loc ~attrs <$> (sub.expr sub e1) <*> (sub.expr sub e2)
    | Pexp_for (p, e1, e2, d, e3) ->
        for_ ~loc ~attrs
          <$> (sub.pat sub p)
          <*> (sub.expr sub e1)
          <*> (sub.expr sub e2)
          <*> (return d)
          <*> (sub.expr sub e3)
    | Pexp_coerce (e, t1, t2) ->
        coerce ~loc ~attrs
          <$> (sub.expr sub e)
          <*> (map_opt (sub.typ sub) t1)
          <*> (sub.typ sub t2)
    | Pexp_constraint (e, t) ->
        constraint_ ~loc ~attrs <$> (sub.expr sub e) <*> (sub.typ sub t)
    | Pexp_send (e, s) ->
        send ~loc ~attrs <$> (sub.expr sub e) <*> (map_loc sub s)
    | Pexp_new lid -> new_ ~loc ~attrs <$> (map_loc sub lid)
    | Pexp_setinstvar (s, e) ->
        setinstvar ~loc ~attrs <$> (map_loc sub s) <*> (sub.expr sub e)
    | Pexp_override sel ->
        override ~loc ~attrs <$> (mmap (map_tuple (map_loc sub) (sub.expr sub)) sel)
    | Pexp_letmodule (s, me, e) ->
        letmodule ~loc ~attrs
          <$> (map_loc sub s)
          <*> (sub.module_expr sub me)
          <*> (sub.expr sub e)
    | Pexp_letexception (cd, e) ->
        letexception ~loc ~attrs
          <$> (sub.extension_constructor sub cd)
          <*> (sub.expr sub e)
    | Pexp_assert e -> assert_ ~loc ~attrs <$> (sub.expr sub e)
    | Pexp_lazy e -> lazy_ ~loc ~attrs <$> (sub.expr sub e)
    | Pexp_poly (e, t) ->
        poly ~loc ~attrs <$> (sub.expr sub e) <*> (map_opt (sub.typ sub) t)
    | Pexp_object cls -> object_ ~loc ~attrs <$> (sub.class_structure sub cls)
    | Pexp_newtype (s, e) ->
        newtype ~loc ~attrs <$> (map_loc sub s) <*> (sub.expr sub e)
    | Pexp_pack me -> pack ~loc ~attrs <$> (sub.module_expr sub me)
    | Pexp_open (o, e) ->
        open_ ~loc ~attrs <$> (sub.open_declaration sub o) <*> (sub.expr sub e)
    | Pexp_letop {let_; ands; body} ->
        letop ~loc ~attrs
          <$> (sub.binding_op sub let_)
          <*> (mmap (sub.binding_op sub) ands)
          <*> (sub.expr sub body)
    | Pexp_extension x -> extension ~loc ~attrs <$> (sub.extension sub x)
    | Pexp_unreachable -> return (unreachable ~loc ~attrs ())

  let map_binding_op sub {pbop_op; pbop_pat; pbop_exp; pbop_loc} =
    let open Exp in
    let open Monad in
    binding_op
      <$> (map_loc sub pbop_op)
      <*> (sub.pat sub pbop_pat)
      <*> (sub.expr sub pbop_exp)
      <*> (sub.location sub pbop_loc)

end

module P = struct
  (* Patterns *)

  let map sub {ppat_desc = desc; ppat_loc = loc; ppat_attributes = attrs; ppat_loc_stack = _} =
    let open Pat in
    let open Monad in
    sub.location sub loc     >>= fun loc ->
    sub.attributes sub attrs >>= fun attrs ->
    match desc with
    | Ppat_any -> return (any ~loc ~attrs ())
    | Ppat_var s -> var ~loc ~attrs <$> (map_loc sub s)
    | Ppat_alias (p, s) -> alias ~loc ~attrs <$> (sub.pat sub p) <*> (map_loc sub s)
    | Ppat_constant c -> constant ~loc ~attrs <$> (sub.constant sub c)
    | Ppat_interval (c1, c2) -> return (interval ~loc ~attrs c1 c2)
    | Ppat_tuple pl -> tuple ~loc ~attrs <$> (mmap (sub.pat sub) pl)
    | Ppat_construct (l, p) ->
        construct ~loc ~attrs <$> (map_loc sub l) <*> (map_opt (sub.pat sub) p)
    | Ppat_variant (l, p) -> variant ~loc ~attrs l <$> (map_opt (sub.pat sub) p)
    | Ppat_record (lpl, cf) ->
        record ~loc ~attrs
          <$> (mmap (map_tuple (map_loc sub) (sub.pat sub)) lpl)
          <*> (return cf)
    | Ppat_array pl -> array ~loc ~attrs <$> (mmap (sub.pat sub) pl)
    | Ppat_or (p1, p2) -> or_ ~loc ~attrs <$> (sub.pat sub p1) <*> (sub.pat sub p2)
    | Ppat_constraint (p, t) -> constraint_ ~loc ~attrs <$> (sub.pat sub p) <*> (sub.typ sub t)
    | Ppat_type s -> type_ ~loc ~attrs <$> (map_loc sub s)
    | Ppat_lazy p -> lazy_ ~loc ~attrs <$> (sub.pat sub p)
    | Ppat_unpack s -> unpack ~loc ~attrs <$> (map_loc sub s)
    | Ppat_open (lid,p) -> open_ ~loc ~attrs <$> (map_loc sub lid) <*> (sub.pat sub p)
    | Ppat_exception p -> exception_ ~loc ~attrs <$> (sub.pat sub p)
    | Ppat_extension x -> extension ~loc ~attrs <$> (sub.extension sub x)
end

module CE = struct
  (* Value expressions for the class language *)

  let map sub {pcl_loc = loc; pcl_desc = desc; pcl_attributes = attrs} =
    let open Cl in
    let open Monad in
    sub.location sub loc     >>= fun loc ->
    sub.attributes sub attrs >>= fun attrs ->
    match desc with
    | Pcl_constr (lid, tys) ->
        constr ~loc ~attrs <$> (map_loc sub lid) <*> (mmap (sub.typ sub) tys)
    | Pcl_structure s ->
        structure ~loc ~attrs <$> (sub.class_structure sub s)
    | Pcl_fun (lab, e, p, ce) ->
        fun_ ~loc ~attrs lab
          <$> (map_opt (sub.expr sub) e)
          <*> (sub.pat sub p)
          <*> (sub.class_expr sub ce)
    | Pcl_apply (ce, l) ->
        apply ~loc ~attrs
          <$> (sub.class_expr sub ce)
          <*> (mmap (map_snd (sub.expr sub)) l)
    | Pcl_let (r, vbs, ce) ->
        let_ ~loc ~attrs r
          <$> (mmap (sub.value_binding sub) vbs)
          <*> (sub.class_expr sub ce)
    | Pcl_constraint (ce, ct) ->
        constraint_ ~loc ~attrs
          <$> (sub.class_expr sub ce)
          <*> (sub.class_type sub ct)
    | Pcl_extension x -> extension ~loc ~attrs <$> (sub.extension sub x)
    | Pcl_open (o, ce) ->
        open_ ~loc ~attrs
          <$> (sub.open_description sub o)
          <*> (sub.class_expr sub ce)

  let map_kind sub =
    let open Monad in
    function
    | Cfk_concrete (o, e) ->
        sub.expr sub e >>= fun e ->
        return (Cfk_concrete (o, e))
    | Cfk_virtual t ->
        sub.typ sub t >>= fun t ->
        return (Cfk_virtual t)

  let map_field sub {pcf_desc = desc; pcf_loc = loc; pcf_attributes = attrs} =
    let open Cf in
    let open Monad in
    sub.location sub loc     >>= fun loc ->
    sub.attributes sub attrs >>= fun attrs ->
    match desc with
    | Pcf_inherit (o, ce, s) ->
        inherit_ ~loc ~attrs o
          <$> (sub.class_expr sub ce)
          <*> (map_opt (map_loc sub) s)
    | Pcf_val (s, m, k) ->
        val_ ~loc ~attrs
          <$> (map_loc sub s)
          <*> (return m)
          <*> (map_kind sub k)
    | Pcf_method (s, p, k) ->
        method_ ~loc ~attrs
          <$> (map_loc sub s)
          <*> (return p)
          <*> (map_kind sub k)
    | Pcf_constraint (t1, t2) ->
        constraint_ ~loc ~attrs
          <$> (sub.typ sub t1)
          <*> (sub.typ sub t2)
    | Pcf_initializer e -> initializer_ ~loc ~attrs <$> (sub.expr sub e)
    | Pcf_attribute x -> attribute ~loc <$> (sub.attribute sub x)
    | Pcf_extension x -> extension ~loc ~attrs <$> (sub.extension sub x)

  let map_structure sub {pcstr_self; pcstr_fields} =
    let open Monad in
    sub.pat sub pcstr_self                  >>= fun pcstr_self ->
    mmap (sub.class_field sub) pcstr_fields >>= fun pcstr_fields ->
    return { pcstr_self = pcstr_self; pcstr_fields = pcstr_fields; }

  let class_infos sub f {pci_virt; pci_params = pl; pci_name; pci_expr;
                         pci_loc; pci_attributes} =
    let open Monad in
    sub.location sub pci_loc          >>= fun loc ->
    sub.attributes sub pci_attributes >>= fun attrs ->
    mmap (map_fst (sub.typ sub)) pl   >>= fun pl ->
    map_loc sub pci_name              >>= fun pci_name ->
    f pci_expr                        >>= fun pci_expr ->
    return (
      Ci.mk ~loc ~attrs
       ~virt:pci_virt
       ~params:pl
        pci_name
        pci_expr
    )

end

(* Now, a generic AST mapper, to be extended to cover all kinds and
   cases of the OCaml grammar.  The default behavior of the mapper is
   the identity. *)

let default_mapper =
  let open Monad in
  {
    constant = C.map;
    structure = (fun this l -> mmap (this.structure_item this) l);
    structure_item = M.map_structure_item;
    module_expr = M.map;
    signature = (fun this l -> mmap (this.signature_item this) l);
    signature_item = MT.map_signature_item;
    module_type = MT.map;
    with_constraint = MT.map_with_constraint;
    class_declaration = (fun this ->
      CE.class_infos this (this.class_expr this)
    );
    class_expr = CE.map;
    class_field = CE.map_field;
    class_structure = CE.map_structure;
    class_type = CT.map;
    class_type_field = CT.map_field;
    class_signature = CT.map_signature;
    class_type_declaration =
      (fun this -> CE.class_infos this (this.class_type this));
    class_description =
      (fun this -> CE.class_infos this (this.class_type this));
    type_declaration = T.map_type_declaration;
    type_kind = T.map_type_kind;
    typ = T.map;
    type_extension = T.map_type_extension;
    type_exception = T.map_type_exception;
    extension_constructor = T.map_extension_constructor;
    value_description = (fun this {pval_name; pval_type; pval_prim; pval_loc; pval_attributes} ->
      map_loc this pval_name               >>= fun pval_name ->
      this.typ this pval_type              >>= fun pval_type ->
      this.attributes this pval_attributes >>= fun pval_attributes ->
      this.location this pval_loc          >>= fun pval_loc ->
      return (Val.mk pval_name pval_type ~attrs:pval_attributes ~loc:pval_loc ~prim:pval_prim)
    );
    pat = P.map;
    expr = E.map;
    binding_op = E.map_binding_op;
    module_declaration = (fun this {pmd_name; pmd_type; pmd_attributes; pmd_loc} ->
      map_loc this pmd_name               >>= fun pmd_name ->
      this.module_type this pmd_type      >>= fun pmd_type ->
      this.attributes this pmd_attributes >>= fun pmd_attributes ->
      this.location this pmd_loc          >>= fun pmd_loc ->
      return (Md.mk pmd_name pmd_type ~attrs:pmd_attributes ~loc:pmd_loc)
    );
    module_substitution = (fun this {pms_name; pms_manifest; pms_attributes; pms_loc} ->
      map_loc this pms_name               >>= fun pms_name ->
      map_loc this pms_manifest           >>= fun pms_manifest ->
      this.attributes this pms_attributes >>= fun pms_attributes ->
      this.location this pms_loc          >>= fun pms_loc ->
      return (Ms.mk pms_name pms_manifest ~attrs:pms_attributes ~loc:pms_loc)
    );
    module_type_declaration = (fun this {pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc} ->
      map_loc this pmtd_name                    >>= fun pmtd_name ->
      map_opt (this.module_type this) pmtd_type >>= fun pmtd_type ->
      this.attributes this pmtd_attributes      >>= fun pmtd_attributes ->
      this.location this pmtd_loc               >>= fun pmtd_loc ->
      return (Mtd.mk pmtd_name ?typ:pmtd_type ~attrs:pmtd_attributes ~loc:pmtd_loc)
    );
    module_binding = (fun this {pmb_name; pmb_expr; pmb_attributes; pmb_loc} ->
      map_loc this pmb_name               >>= fun pmb_name ->
      this.module_expr this pmb_expr      >>= fun pmb_expr ->
      this.attributes this pmb_attributes >>= fun pmb_attributes ->
      this.location this pmb_loc          >>= fun pmb_loc ->
      return (Mb.mk pmb_name pmb_expr ~attrs:pmb_attributes ~loc:pmb_loc)
    );
    open_declaration = (fun this {popen_expr; popen_override; popen_attributes; popen_loc} ->
      this.module_expr this popen_expr      >>= fun popen_expr ->
      this.location this popen_loc          >>= fun popen_loc ->
      this.attributes this popen_attributes >>= fun popen_attributes ->
      return (Opn.mk popen_expr ~override:popen_override ~loc:popen_loc ~attrs:popen_attributes)
    );
    open_description = (fun this {popen_expr; popen_override; popen_attributes; popen_loc} ->
      map_loc this popen_expr               >>= fun popen_expr ->
      this.location this popen_loc          >>= fun popen_loc ->
      this.attributes this popen_attributes >>= fun popen_attributes ->
      return (Opn.mk popen_expr ~override:popen_override ~loc:popen_loc ~attrs:popen_attributes)
    );
    include_description = (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
      this.module_type this pincl_mod       >>= fun pincl_mod ->
      this.location this pincl_loc          >>= fun pincl_loc ->
      this.attributes this pincl_attributes >>= fun pincl_attributes ->
      return (Incl.mk pincl_mod ~loc:pincl_loc ~attrs:pincl_attributes)
    );
    include_declaration = (fun this {pincl_mod; pincl_attributes; pincl_loc} ->
      this.module_expr this pincl_mod       >>= fun pincl_mod ->
      this.location this pincl_loc          >>= fun pincl_loc ->
      this.attributes this pincl_attributes >>= fun pincl_attributes ->
      return (Incl.mk pincl_mod ~loc:pincl_loc ~attrs:pincl_attributes)
    );
    value_binding = (fun this {pvb_pat; pvb_expr; pvb_attributes; pvb_loc} ->
      this.pat this pvb_pat               >>= fun pvb_pat ->
      this.expr this pvb_expr             >>= fun pvb_expr ->
      this.location this pvb_loc          >>= fun pvb_loc ->
      this.attributes this pvb_attributes >>= fun pvb_attributes ->
      return (Vb.mk pvb_pat pvb_expr ~loc:pvb_loc ~attrs:pvb_attributes)
    );
    constructor_declaration = (fun this {pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes} ->
      map_loc this pcd_name                     >>= fun pcd_name ->
      T.map_constructor_arguments this pcd_args >>= fun pcd_args ->
      map_opt (this.typ this) pcd_res           >>= fun pcd_res ->
      this.location this pcd_loc                >>= fun pcd_loc ->
      this.attributes this pcd_attributes       >>= fun pcd_attributes ->
      return (
        Type.constructor
          pcd_name
          ~args:pcd_args
          ?res:pcd_res
          ~loc:pcd_loc
          ~attrs:pcd_attributes
      )
    );
    label_declaration = (fun this {pld_name; pld_type; pld_loc; pld_mutable; pld_attributes} ->
      map_loc this pld_name               >>= fun pld_name ->
      this.typ this pld_type              >>= fun pld_type ->
      this.location this pld_loc          >>= fun pld_loc ->
      this.attributes this pld_attributes >>= fun pld_attributes ->
      return (
        Type.field
          pld_name
          pld_type
          ~mut:pld_mutable
          ~loc:pld_loc
          ~attrs:pld_attributes
      )
    );
    cases = (fun this l ->
      mmap (this.case this) l
    );
    case = (fun this {pc_lhs; pc_guard; pc_rhs} ->
      this.pat this pc_lhs              >>= fun pc_lhs ->
      map_opt (this.expr this) pc_guard >>= fun pc_guard ->
      this.expr this pc_rhs             >>= fun pc_rhs ->
      return { pc_lhs = pc_lhs; pc_guard = pc_guard; pc_rhs = pc_rhs; }
    );
    location = (fun _this l ->
      return l
    );
    extension = (fun this (s, e) ->
      map_loc this s      >>= fun s ->
      this.payload this e >>= fun e ->
      return (s, e)
    );
    attribute = (fun this a ->
      map_loc this a.attr_name         >>= fun attr_name ->
      this.payload this a.attr_payload >>= fun attr_payload ->
      this.location this a.attr_loc    >>= fun attr_loc ->
      return { attr_name = attr_name; attr_payload = attr_payload; attr_loc = attr_loc; }
    );
    attributes = (fun this l ->
      mmap (this.attribute this) l
    );
    payload = (fun this ->
      function
      | PStr x -> this.structure this x >>= fun x -> return (PStr x)
      | PSig x -> this.signature this x >>= fun x -> return (PSig x)
      | PTyp x -> this.typ this x >>= fun x -> return (PTyp x)
      | PPat (x, g) ->
        this.pat this x            >>= fun x ->
        map_opt (this.expr this) g >>= fun g ->
        return (PPat (x, g))
    );
  }

*)
