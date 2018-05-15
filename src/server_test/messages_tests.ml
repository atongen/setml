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
        make_board_card_data 1 12;
        make_board_card_data 2 11;
        make_board_card_data 3 10;
        make_board_card_data 4 9;
        make_board_card_data 5 8;
        make_board_card_data 6 7;
        make_board_card_data 7 6;
        make_board_card_data 8 5;
        make_board_card_data 9 4;
        make_board_card_data 10 3;
        make_board_card_data 11 2;
        make_board_card_data 12 1;
    ];
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
  ] in
  let batch = Messages.make_batch messages
  in
  cases_of check (messages @ [batch])

let suite = [
  "convert_tests" >::: convert_tests;
]
