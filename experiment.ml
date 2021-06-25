
let parse_ml_file (src_file : string) : Parsetree.structure =
  Pparse.parse_implementation ~tool_name:"ocamlc" src_file

let _ =
  (* one file at a time *)
  let ast = parse_ml_file Sys.argv.(1) in
  Format.fprintf Format.std_formatter "%a@." Printast.implementation ast;
  ()
