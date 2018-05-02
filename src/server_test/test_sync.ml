open OUnit2

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
    "pubsub" >::: Pubsub_tests.suite;
  ]
