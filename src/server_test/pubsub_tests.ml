open OUnit2
open Shared
open Shared.Messages

open Lib
open Lib.Server_messages

open Test_util

let pubsub_tests pubsub =
    let game_id = 1_679_616 in
    let player_id = 1 in
    let player_name = "bob" in
    let check (query, expChan, expMsg) =
        fun test_ctx ->
            Pubsub.empty_query pubsub ("delete from games where id = " ^ string_of_int game_id ^ ";");
            Pubsub.empty_query pubsub ("delete from players where id = " ^ string_of_int player_id ^ ";");
            Pubsub.empty_query pubsub ("insert into games (id) values (" ^ string_of_int game_id ^ ");");
            Pubsub.empty_query pubsub ("insert into players (id, name) values (" ^ string_of_int player_id ^ ", '"^ player_name ^"');");
            Pubsub.empty_query pubsub ("listen game_" ^ string_of_int game_id ^ ";");
            Pubsub.empty_query pubsub query;
            let i = Pubsub.get_notifications pubsub (fun pubsub channel pid payload ->
                assert_equal ~ctxt:test_ctx expChan channel;
                let gotMsg = Server_message_converter.of_json payload in
                assert_equal ~ctxt:test_ctx expMsg gotMsg ~printer:Messages.to_string
            ) in
            assert_equal ~ctxt:test_ctx ~printer:string_of_int 1 i;
            Pubsub.empty_query pubsub ("unlisten game_" ^ string_of_int game_id ^ ";");
    in
    cases_of check [
        ((Printf.sprintf "insert into games_players (game_id, player_id, presence) values (%d, %d, true);" game_id player_id), "game_" ^ string_of_int game_id, (Messages.make_presence game_id player_id player_name true));
    ]


let suite =
    let clients = Clients.make () in
    let pubsub = Pubsub.make "user=atongen password=at1234 port=5435 host=localhost dbname=setml_development" clients in
    [
        "pubsub" >::: pubsub_tests pubsub
    ]
