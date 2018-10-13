open OUnit2
open Lib
(*
 * OUnit2 test suite of tests that must be run in serial
 * ie bin/test -runner sequential
 *)

let suite pubsub =
  "All" >::: [
    "pubsub" >::: Pubsub_tests.suite pubsub;
  ]

let run (config: Config.t) =
    let clients = Clients.make () in
    match Pubsub.make (Config.db_conninfo config) clients with
    | Ok pubsub -> OUnit2.run_test_tt_main (suite pubsub)
    | Error msg -> failwith msg

let () =
    match Config.parse () with
    | `Ok c -> run c
    | r -> Cmdliner.Term.exit r
