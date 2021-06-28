
let _ =
  (* one file at a time *)
  let _ = Mutator.reformat_ast Sys.argv.(1) in
(*
  let ast = Mutator.parse_ml_file Sys.argv.(1) in
  Mutator.print_ast ast;
*)
  ()
