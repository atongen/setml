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

let () =
    let config = Config.make_of_env () in
    let clients = Clients.make () in
    match Pubsub.make (Config.db_conninfo config) clients with
    | Ok pubsub -> OUnit2.run_test_tt_main (suite pubsub)
    | Error msg -> failwith msg
