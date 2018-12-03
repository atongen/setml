open OUnit2
open Shared
open Shared.Api_messages

open Lib.Server_api_messages

open Test_lib.Test_util

let convert_player_games_tests =
    let p pgs = String.concat ", " (List.map Api_messages.player_game_to_string pgs) in
    let check v =
        let open Server_api_message_converter in
        let d = v |> player_games_to_json |> player_games_of_json in
        ae v d ~printer:p
    in
    let player_games = [
        [
            make_player_game 7 Game_status.New 13 1543797509.5927184;
            make_player_game 11 Game_status.Started 17 1543797519.5977184;
            make_player_game 13 Game_status.Complete 19 1543797719.7977184;
        ]
    ]
    in
    cases_of check player_games

let suite = [
    "player_games" >::: convert_player_games_tests;
]
