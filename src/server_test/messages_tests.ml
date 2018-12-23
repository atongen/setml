open OUnit2
open Shared
open Shared.Messages

open Lib.Server_messages

open Test_lib.Test_util

let convert_tests =
  let check v =
    let open Server_message_converter in
    let d = v |> to_json |> of_json in
    ae v d ~printer:Messages.to_string
  in
  let messages = [
    make_server_game [
        make_player_data 1 "john" true 5 1;
        make_player_data 2 "rich" true 4 2;
        make_player_data 3 "bill" false 6 3;
        make_player_data 4 "cindy" true 9 4;
        make_player_data 5 "carol" true 3 5;
    ] (make_board_cards_data [
         0;  2;  4;  6;
         8; 10; 33; 52;
        78; 79; 80; 81;
    ]) (make_game_update_data 15 "started" "classic" 3 4 None);
    make_server_name 2 "andrew2";
    make_server_card 5 7;
    make_server_board_card 5 7;
    make_server_game_update 13 "new" "classic" 2 2 None;
    make_server_game_update 17 "started" "classic" 2 3 None;
    make_server_game_update 19 "complete" "classic" 2 4 None;
    make_server_game_update 21 "new" "open_source" 3 2 None;
    make_server_game_update 23 "started" "open_source" 3 3 None;
    make_server_game_update 25 "complete" "open_source" 3 4 None;
    make_server_game_update 21 "new" "open_source" 4 2 None;
    make_server_game_update 23 "started" "open_source" 4 3 None;
    make_server_game_update 25 "complete" "open_source" 4 4 None;
    make_server_game_update 13 "new" "classic" 2 2 (Some 1);
    make_server_game_update 17 "started" "classic" 2 3 (Some 7);
    make_server_game_update 19 "complete" "classic" 2 4 (Some 9);
    make_server_game_update 21 "new" "open_source" 3 2 (Some 13);
    make_server_game_update 23 "started" "open_source" 3 3 (Some 17);
    make_server_game_update 25 "complete" "open_source" 3 4 (Some 23);
    make_server_game_update 21 "new" "open_source" 4 2 (Some 49);
    make_server_game_update 23 "started" "open_source" 4 3 (Some 111);
    make_server_game_update 25 "complete" "open_source" 4 4 (Some 23423334678855333);
    make_server_game_update 21 "new" "hero" 3 2 None;
    make_server_game_update 23 "started" "hero" 3 3 None;
    make_server_game_update 25 "complete" "hero" 3 4 None;
    make_server_game_update 21 "new" "hero" 4 2 None;
    make_server_game_update 23 "started" "hero" 4 3 None;
    make_server_game_update 25 "complete" "hero" 4 4 None;
    make_server_game_update 13 "new" "classic" 2 2 (Some 1);
    make_server_game_update 17 "started" "classic" 2 3 (Some 7);
    make_server_game_update 19 "complete" "classic" 2 4 (Some 9);
    make_server_game_update 21 "new" "hero" 3 2 (Some 13);
    make_server_game_update 23 "started" "hero" 3 3 (Some 17);
    make_server_game_update 25 "complete" "hero" 3 4 (Some 23);
    make_server_game_update 21 "new" "hero" 4 2 (Some 49);
    make_server_game_update 23 "started" "hero" 4 3 (Some 111);
    make_server_game_update 25 "complete" "hero" 4 4 (Some 23423334678855333);
    make_server_score 6 4;
    make_server_move (1, 10) (2, 20) (3, 30);
    make_server_move (4, 11) (5, 21) (6, 31);
    make_server_move (7, 64) (8, 77) (9, 80);
    make_server_presence 1 true;
    make_server_presence 1 false;
    make_server_move_info (make_score_data 5 15) (make_move_data (1,1) (2,2) (3, 3));
    make_server_shuffles 4 6;
    make_server_player 6 "willie" true 9 8;
    make_client_move "wow" (make_move_data (1,1) (2,2) (3, 3));
    make_client_shuffle "my-token";
    make_client_start_game "my-other-token";
    make_client_name "some weird token" "some weird name";
    make_client_theme "classic token" Theme.Classic;
    make_client_theme "open source token" Theme.Open_source;
    make_client_theme "other token" Theme.Hero;
  ]
  in
  cases_of check messages

let suite = [
  "convert_tests" >::: convert_tests;
]
