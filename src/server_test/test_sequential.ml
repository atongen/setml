open OUnit2
open Lib
(*
 * OUnit2 test suite of tests that must be run in serial
 * ie bin/test -runner sequential
 *)

let suite (config: Config.t) =
  "All" >::: [
    "pubsub" >::: Pubsub_tests.suite config;
  ]

let run (config: Config.t) =
    let s = suite config in
    OUnit2.run_test_tt_main s

let () =
    match Config.parse () with
    | `Ok c -> run c
    | r -> Cmdliner.Term.exit r
