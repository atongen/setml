open OUnit2
(*
 * OUnit2 test suite of tests that must be run in serial
 * ie bin/test -runner sequential
 *)

let suite =
  "All" >::: [
    "pubsub" >::: Pubsub_tests.suite;
  ]

let _ = OUnit2.run_test_tt_main suite
