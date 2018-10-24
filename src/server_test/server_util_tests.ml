open OUnit2
open Shared

open Test_lib.Test_util

let roundi_tests =
    let check (v, exp) =
        let got = Shared_util.roundi v in
        ae ~printer:string_of_int exp got
    in
    cases_of check [
        (-1.0, -1);
        (-0.9, -1);
        (-0.6, -1);
        (-0.5, -1);
        (-0.4, 0);
        (-0.1, 0);
        (0.0, 0);
        (0.1, 0);
        (0.4, 0);
        (0.5, 1);
        (0.6, 1);
        (0.9, 1);
        (1.0, 1);
    ]

let suite = [
    "roundi_tests" >::: roundi_tests;
]
