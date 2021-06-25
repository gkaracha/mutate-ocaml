
let parse_ml_file (src_file : string) : Parsetree.structure =
  Pparse.parse_implementation ~tool_name:"ocamlc" src_file

let _ =
  (* one file at a time *)
  let ast = Mutator.parse_ml_file Sys.argv.(1) in
  Mutator.print_ast ast;
  ()
