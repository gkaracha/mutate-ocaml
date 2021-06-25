open OUnit2

let _ = Mutator.print_ast (Mutator.parse_ml_file "../src/mutator/mutator.ml")
let _ = Mutator.print_raw_ast (Mutator.parse_ml_file "../src/mutator/mutator.ml")

let suite =
  "MutatorTests" >::: [
    (* Add tests here *)
  ]

let () =
  run_test_tt_main
    suite
