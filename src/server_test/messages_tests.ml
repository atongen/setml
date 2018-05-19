open OUnit2
open Shared
open Shared.Messages

open Lib
open Lib.Server_messages

open Test_lib.Test_util

let convert_tests =
  let check v =
    let open Server_message_converter in
    let d = v |> to_json |> of_json in
    ae v d ~printer:Messages.to_string
  in
  let messages = [
    make_scoreboard [
        make_scoreboard_player_data 1 "john" true 5;
        make_scoreboard_player_data 2 "rich" true 4;
        make_scoreboard_player_data 3 "bill" false 6;
        make_scoreboard_player_data 4 "cindy" true 9;
        make_scoreboard_player_data 5 "carol" true 3;
    ] [
        Card.of_int 3;
        Card.of_int 6;
        Card.of_int 9;
        Card.of_int 12;
        Card.of_int 15;
        Card.of_int 18;
        Card.of_int 21;
        Card.of_int 24;
        Card.of_int 27;
        Card.of_int 30;
        Card.of_int 33;
        Card.of_int 36;
    ] (game_status_data_of_string "pending");
    make_player_name 2 "andrew2";
    make_board_card 5 7;
    make_game_card_idx 13;
    make_game_status "new";
    make_game_status "pending";
    make_game_status "started";
    make_game_status "complete";
    make_score 6 4;
    make_previous_move 10 20 30;
    make_previous_move 11 21 31;
    make_previous_move 64 77 80;
    make_player_presence 1 true;
    make_player_presence 1 false;
  ] in
  let batch = Messages.make_batch messages
  in
  cases_of check (messages @ [batch])

let suite = [
  "convert_tests" >::: convert_tests;
]
