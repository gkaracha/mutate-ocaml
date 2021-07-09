(* A generic Parsetree monadic mapping class *)

(* This module implements a lifted version (generalization) of
 * ocaml-4.12.0/parsing/ast_mapper.ml where mappers operate within a monad. *)

open Parsetree

type mapper = {
  attribute: mapper -> attribute -> attribute Monad.t;
  attributes: mapper -> attribute list -> (attribute list) Monad.t;
  binding_op: mapper -> binding_op -> binding_op Monad.t;
  case: mapper -> case -> case Monad.t;
  cases: mapper -> case list -> (case list) Monad.t;
  class_declaration: mapper -> class_declaration -> class_declaration Monad.t;
  class_description: mapper -> class_description -> class_description Monad.t;
  class_expr: mapper -> class_expr -> class_expr Monad.t;
  class_field: mapper -> class_field -> class_field Monad.t;
  class_signature: mapper -> class_signature -> class_signature Monad.t;
  class_structure: mapper -> class_structure -> class_structure Monad.t;
  class_type: mapper -> class_type -> class_type Monad.t;
  class_type_declaration: mapper -> class_type_declaration -> class_type_declaration Monad.t;
  class_type_field: mapper -> class_type_field -> class_type_field Monad.t;
  constant: mapper -> constant -> constant Monad.t;
  constructor_declaration: mapper -> constructor_declaration -> constructor_declaration Monad.t;
  expr: mapper -> expression -> expression Monad.t;
  extension: mapper -> extension -> extension Monad.t;
  extension_constructor: mapper -> extension_constructor -> extension_constructor Monad.t;
  include_declaration: mapper -> include_declaration -> include_declaration Monad.t;
  include_description: mapper -> include_description -> include_description Monad.t;
  label_declaration: mapper -> label_declaration -> label_declaration Monad.t;
  location: mapper -> Location.t -> Location.t Monad.t;
  module_binding: mapper -> module_binding -> module_binding Monad.t;
  module_declaration: mapper -> module_declaration -> module_declaration Monad.t;
  module_substitution: mapper -> module_substitution -> module_substitution Monad.t;
  module_expr: mapper -> module_expr -> module_expr Monad.t;
  module_type: mapper -> module_type -> module_type Monad.t;
  module_type_declaration: mapper -> module_type_declaration -> module_type_declaration Monad.t;
  open_declaration: mapper -> open_declaration -> open_declaration Monad.t;
  open_description: mapper -> open_description -> open_description Monad.t;
  pat: mapper -> pattern -> pattern Monad.t;
  payload: mapper -> payload -> payload Monad.t;
  signature: mapper -> signature -> signature Monad.t;
  signature_item: mapper -> signature_item -> signature_item Monad.t;
  structure: mapper -> structure -> structure Monad.t;
  structure_item: mapper -> structure_item -> structure_item Monad.t;
  typ: mapper -> core_type -> core_type Monad.t;
  type_declaration: mapper -> type_declaration -> type_declaration Monad.t;
  type_extension: mapper -> type_extension -> type_extension Monad.t;
  type_exception: mapper -> type_exception -> type_exception Monad.t;
  type_kind: mapper -> type_kind -> type_kind Monad.t;
  value_binding: mapper -> value_binding -> value_binding Monad.t;
  value_description: mapper -> value_description -> value_description Monad.t;
  with_constraint: mapper -> with_constraint -> with_constraint Monad.t;
}

(** Now, a generic AST mapper, to be extended to cover all kinds and cases of
  * the OCaml grammar.  The default behavior of the mapper is the identity. *)
val default_mapper : mapper
