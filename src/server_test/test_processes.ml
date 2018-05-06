open OUnit2
(*
 * OUnit2 test suite of tests that can be run in parallel
 * ie bin/test -runner processes
 *)

let suite =
  "All" >::: [
    "crypto" >::: Crypto_tests.suite;
    "clients" >::: Clients_tests.suite;
    "server_util" >::: Server_util_tests.suite;
    "shared" >::: [
      "card" >::: Card_tests.suite;
      "combinatorics" >::: Combinatorics_tests.suite;
    ];
    "messages" >::: Messages_tests.suite;
  ]

let _ = OUnit2.run_test_tt_main suite
