open OUnit2
open Lib
open Shared

open Test_lib.Test_util

let choose_tests =
  let check (n, k, exp) =
    let got = Combinatorics.choose n k in
    aie exp got
  in
  cases_of check [
    (1, 1, 1);
    (2, 2, 1);
    (10, 4, 210);
    (100, 4, 3921225);
    (13, 7, 1716);
    (45, 10, 3_190_187_286);
  ]

let generator_tests =
  let l = [0; 1; 2; 3; 4; 5;] in
  let g = Combinatorics.comb_generator l 2 in
  let check exp =
    match exp with
    | Some (exp) ->
      (match g () with
       | Some (got) -> ae ~printer:string_of_int_list exp got
       | None -> af "Expected another triple to be generated!")
    | None ->
      (match g () with
       | Some (got) -> af ("Expected none to be generated, but got: " ^ (string_of_int_list got))
       | None -> ab "none" true)
  in
  cases_of check [
    Some [0;1];
    Some [0;2];
    Some [1;2];
    Some [0;3];
    Some [1;3];
    Some [2;3];
    Some [0;4];
    Some [1;4];
    Some [2;4];
    Some [3;4];
    Some [0;5];
    Some [1;5];
    Some [2;5];
    Some [3;5];
    Some [4;5];
    None;
  ]

  let suite = [
    "choose_test" >::: choose_tests;
    "generator_test" >::: generator_tests;
  ]
