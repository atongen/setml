open OUnit2
open Shared
open Shared.Messages

open Lib
open Lib.Server_messages

open Test_util

let convert_tests =
  let check v =
    let e = Server_message_converter.to_json v in
    let d = Server_message_converter.of_json e in
    ae v d ~printer:Messages.to_string
  in
  cases_of check [
    Messages.make_presence 321 1 "andrew" true;
    Messages.make_player_name 123 2 "andrew2";
  ]

let suite = [
  "convert_tests" >::: convert_tests;
]
