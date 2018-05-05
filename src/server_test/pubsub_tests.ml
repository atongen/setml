open OUnit2
open Shared
open Shared.Messages

open Lib
open Lib.Server_messages

open Test_util

let setup pubsub =
    let game_id = rand_int 1_679_616 60_466_175 in
    let player_id = rand_int 1_000 1_000_000 in
    let player_name = Crypto.random_hex () in
    Pubsub.empty_query pubsub ("insert into games (id) values (" ^ string_of_int game_id ^ ");");
    Pubsub.empty_query pubsub ("insert into players (id, name) values (" ^ string_of_int player_id ^ ", '"^ player_name ^"');");
    Pubsub.empty_query pubsub ("listen game_" ^ string_of_int game_id ^ ";");
    (game_id, player_id, player_name)

let teardown pubsub game_id player_id =
    Pubsub.empty_query pubsub ("unlisten game_" ^ string_of_int game_id ^ ";");
    Pubsub.empty_query pubsub ("delete from games where id = " ^ string_of_int game_id ^ ";");
    Pubsub.empty_query pubsub ("delete from players where id = " ^ string_of_int player_id ^ ";");
    ()

let make_presence_query game_id player_id present =
    Printf.sprintf
        {eos|
            insert into games_players (game_id, player_id, presence, updated_at)
            values (%d, %d, %B, now())
            on conflict (game_id, player_id)
            do update set presence = excluded.presence,
            updated_at = excluded.updated_at;
        |eos}
    game_id player_id present

let presence_check pubsub game_id player_id player_name test_ctx =
    List.iter (fun present ->
        Pubsub.empty_query pubsub (make_presence_query game_id player_id present);
        let msgs = Pubsub.get_notifications pubsub in
        assert_equal ~printer:string_of_int 1 (List.length msgs);
        let expMsg = Messages.make_presence game_id player_id player_name present in
        let json = (List.hd msgs).extra in
        let gotMsg = Server_message_converter.of_json json in
        assert_equal ~ctxt:test_ctx expMsg gotMsg ~printer:Messages.to_string
    ) [true; false]

let presence_check_accum pubsub game_id player_id player_name test_ctx =
    Pubsub.empty_query pubsub (make_presence_query game_id player_id true);
    Pubsub.empty_query pubsub (make_presence_query game_id player_id false);
    let msgs = Pubsub.get_notifications pubsub in
    assert_equal ~printer:string_of_int 2 (List.length msgs);
    let msgs_arr = Array.of_list msgs in
    let expMsg0 = Messages.make_presence game_id player_id player_name true in
    let expMsg1 = Messages.make_presence game_id player_id player_name false in
    let json0 = msgs_arr.(0).extra in
    let json1 = msgs_arr.(1).extra in
    let gotMsg0 = Server_message_converter.of_json json0 in
    let gotMsg1 = Server_message_converter.of_json json1 in
    assert_equal ~ctxt:test_ctx expMsg0 gotMsg0 ~printer:Messages.to_string;
    assert_equal ~ctxt:test_ctx expMsg1 gotMsg1 ~printer:Messages.to_string

(* this test should check for messages on multiple game channels when a single player name changes
let player_name_case name =
    let game_id, player_id, player_name = general_case () in
    let query = Printf.sprintf "update players set name = '%s' where id = %d;" name player_id in
    let channel = "game_" ^ string_of_int game_id in
    let msg = Messages.make_player_name game_id player_id player_name in
    (game_id, player_id, player_name, query, channel, msg)
*)

let pubsub_tests pubsub =
    let check f =
        fun test_ctx ->
            let game_id, player_id, player_name = setup pubsub in
            f pubsub game_id player_id player_name test_ctx;
            teardown pubsub game_id player_id;
    in
    cases_of check [
        presence_check;
        presence_check_accum;
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
