open OUnit2
open Shared

open Test_lib.Test_util

let deck = Card.deck ()

let triple_to_cards = function
    (a, b, c) -> (Card.of_int a, Card.of_int b, Card.of_int c)

(* all combinations of first 9 cards *)
let set_yes_list = [
  (0, 1, 2); (0, 3, 6); (0, 4, 8);
  (1, 3, 8); (1, 4, 7); (1, 5, 6);
  (2, 3, 7); (2, 4, 6); (2, 5, 8);
  (3, 4, 5); (6, 7, 8); (0, 5, 7);
]

let set_yes_cards = List.map triple_to_cards set_yes_list

let set_no_list = [
  (0, 1, 3); (0, 1, 4); (0, 1, 5);
  (0, 1, 6); (0, 1, 7); (0, 1, 8);
  (0, 2, 3); (0, 2, 4); (0, 2, 5);
  (0, 2, 6); (0, 2, 7); (0, 2, 8);
  (0, 3, 4); (0, 3, 5); (0, 3, 7);
  (0, 3, 8); (0, 4, 5); (0, 4, 6);
  (0, 4, 7); (0, 5, 6); (0, 5, 8);
  (0, 6, 7); (0, 6, 8); (0, 7, 8);
  (1, 2, 3); (1, 2, 4); (1, 2, 5);
  (1, 2, 6); (1, 2, 7); (1, 2, 8);
  (1, 3, 4); (1, 3, 5); (1, 3, 6);
  (1, 3, 7); (1, 4, 5); (1, 4, 6);
  (1, 4, 8); (1, 5, 7); (1, 5, 8);
  (1, 6, 7); (1, 6, 8); (1, 7, 8);
  (2, 3, 4); (2, 3, 5); (2, 3, 6);
  (2, 3, 8); (2, 4, 5); (2, 4, 7);
  (2, 4, 8); (2, 5, 6); (2, 5, 7);
  (2, 6, 7); (2, 6, 8); (2, 7, 8);
  (3, 4, 6); (3, 4, 7); (3, 4, 8);
  (3, 5, 6); (3, 5, 7); (3, 5, 8);
  (3, 6, 7); (3, 6, 8); (3, 7, 8);
  (4, 5, 6); (4, 5, 7); (4, 5, 8);
  (4, 6, 7); (4, 6, 8); (4, 7, 8);
  (5, 6, 7); (5, 6, 8); (5, 7, 8);
]

let set_no_cards = List.map triple_to_cards set_no_list

let cards_tests =
  let open Card.Infix in
  [
    test_case (aie 81 (Array.length deck));

    test_case (ase "{ n: 0, f: 0, c: 0, s: 0 }" (Card.to_string (deck.(0))));

    test_case (ase "{ n: 0, f: 0, c: 0, s: 1 }" (Card.to_string (deck.(1))));
    test_case (ase "{ n: 0, f: 0, c: 0, s: 2 }" (Card.to_string (deck.(2))));

    test_case (ase "{ n: 0, f: 0, c: 1, s: 0 }" (Card.to_string (deck.(3))));
    test_case (ase "{ n: 0, f: 0, c: 2, s: 0 }" (Card.to_string (deck.(6))));

    test_case (ase "{ n: 0, f: 1, c: 0, s: 0 }" (Card.to_string (deck.(9))));
    test_case (ase "{ n: 0, f: 2, c: 0, s: 0 }" (Card.to_string (deck.(18))));

    test_case (ase "{ n: 1, f: 0, c: 0, s: 0 }" (Card.to_string (deck.(27))));
    test_case (ase "{ n: 2, f: 0, c: 0, s: 0 }" (Card.to_string (deck.(54))));

    test_case (ase "{ n: 2, f: 2, c: 2, s: 2 }" (Card.to_string (deck.(80))));

    test_case (aie (List.length set_yes_list) (Card.count_sets (0 --^ 9)));
    test_case (aie (List.length set_no_list) (Card.count_non_sets (0 --^ 9)));
  ]

let set_desc prefix c0 c1 c2 =
  let str_lst = List.map (fun c -> Card.to_string c) [c0; c1; c2] in
  prefix ^ ": " ^ String.concat ", " str_lst

let cards_complete_sets_tests =
  let check(idx0, idx1) =
    let c0 = deck.(idx0) in
    let c1 = deck.(idx1) in
    let c2 = Card.complete c0 c1 in
    let desc = set_desc "expected set: " c0 c1 c2 in
    let s = Card.is_set c0 c1 c2 in
    ab desc s
  in
  let cases =
    let open CCList.Infix in
    0 -- 100 >|= (fun _ ->
        (Random.int(81), Random.int(81))
      )
  in
  cases_of check cases

let cards_is_set_tests =
  let open CCList.Infix in
  let check (idx0, idx1, idx2, exp) =
    let (c0, c1, c2) = (deck.(idx0), deck.(idx1), deck.(idx2)) in
    let s = Card.is_set c0 c1 c2 in
    let str = if exp then "expected YES set: " else "expected NO set: " in
    let desc = set_desc str c0 c1 c2 in
    ab desc (s == exp)
  in
  let idx_triple_to_case v = function (a, b, c) -> (a, b, c, v) in
  let set_cases = [
    (set_yes_list >|= idx_triple_to_case true);
    (set_no_list >|= idx_triple_to_case false);
  ] in
  let cases = CCList.fold_right (@) set_cases [] in
  cases_of check cases

let suite = [
    "tests" >::: cards_tests;
    "is_set" >::: cards_is_set_tests;
    "completion" >::: cards_complete_sets_tests;
]
