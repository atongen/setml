open OUnit2
open Shared
open Shared.Messages

open Lib
open Lib.Server_messages

open Test_util

let setup pubsub game_id player_id player_name =
    Pubsub.empty_query pubsub ("insert into games (id) values (" ^ string_of_int game_id ^ ");");
    Pubsub.empty_query pubsub ("insert into players (id, name) values (" ^ string_of_int player_id ^ ", '"^ player_name ^"');");
    Pubsub.empty_query pubsub ("listen game_" ^ string_of_int game_id ^ ";");
    ()

let teardown pubsub game_id player_id =
    Pubsub.empty_query pubsub ("unlisten game_" ^ string_of_int game_id ^ ";");
    Pubsub.empty_query pubsub ("delete from games where id = " ^ string_of_int game_id ^ ";");
    Pubsub.empty_query pubsub ("delete from players where id = " ^ string_of_int player_id ^ ";");
    ()

let general_case () =
    let game_id = rand_int 1_679_616 60_466_175 in
    let player_id = rand_int 1_000 1_000_000 in
    let player_name = Crypto.random_hex () in
    (game_id, player_id, player_name)

(* this test should toggle a single user presense true/false *)
let presence_case present =
    let game_id, player_id, player_name = general_case () in
    let query = Printf.sprintf "insert into games_players (game_id, player_id, presence) values (%d, %d, %B);" game_id player_id present in
    let channel = "game_" ^ string_of_int game_id in
    let msg = Messages.make_presence game_id player_id player_name present in
    (game_id, player_id, player_name, query, channel, msg)

(* this test should check for messages on multiple game channels when a single player name changes *)
let player_name_case name =
    let game_id, player_id, player_name = general_case () in
    let query = Printf.sprintf "update players set name = '%s' where id = %d;" name player_id in
    let channel = "game_" ^ string_of_int game_id in
    let msg = Messages.make_player_name game_id player_id player_name in
    (game_id, player_id, player_name, query, channel, msg)

let pubsub_tests pubsub =
    let check (game_id, player_id, player_name, query, expChan, expMsg) =
        fun test_ctx ->
            setup pubsub game_id player_id player_name;
            Pubsub.empty_query pubsub query;
            let i = Pubsub.get_notifications pubsub (fun pubsub channel pid payload ->
                assert_equal ~ctxt:test_ctx expChan channel;
                let gotMsg = Server_message_converter.of_json payload in
                assert_equal ~ctxt:test_ctx expMsg gotMsg ~printer:Messages.to_string
            ) in
            assert_equal ~ctxt:test_ctx ~printer:string_of_int 1 i;
            teardown pubsub game_id player_id;
    in
    cases_of check [
        presence_case true;
        presence_case false;
        player_name_case "bob";
    ]

(*
 * See bin/test -help
 * bin/test -runner sequential
 * libpq / postgresql library cannot process multiple requests
 * concurrently against a single db connection
 *)
let suite =
    let clients = Clients.make () in
    let pubsub = Pubsub.make "user=atongen password=at1234 port=5435 host=localhost dbname=setml_test" clients in
    pubsub_tests pubsub
