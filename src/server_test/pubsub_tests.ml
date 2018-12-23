open OUnit2
open Shared
open Shared.Messages

open Lib
open Lib.Server_messages

open Test_lib.Test_util

let create_game pubsub =
    let game_id = Crypto.random_int 1_679_616 60_466_175 in
    Pubsub.empty_query pubsub ("insert into games (id) values (" ^ string_of_int game_id ^ ");");
    game_id

let setup_game pubsub =
    let game_id = create_game pubsub in
    Pubsub.empty_query pubsub ("listen game_" ^ string_of_int game_id ^ ";");
    game_id

let setup_player pubsub =
    let player_id = Crypto.random_int 1_000 1_000_000 in
    let player_name = String.sub (Crypto.random_hex ()) 0 16 in
    Pubsub.empty_query pubsub ("insert into players (id, name) values (" ^ string_of_int player_id ^ ", '"^ player_name ^"');");
    (player_id, player_name)

let teardown_game pubsub game_id =
    Pubsub.empty_query pubsub ("unlisten game_" ^ string_of_int game_id ^ ";");
    Pubsub.empty_query pubsub ("delete from games where id = " ^ string_of_int game_id ^ ";")

let teardown_player pubsub player_id =
    Pubsub.empty_query pubsub ("delete from players where id = " ^ string_of_int player_id ^ ";")

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

let make_board_cards_query game_id =
    let open CCList.Infix in
    let rows = List.map (fun i ->
        Printf.sprintf "(%d,%d,%d)" game_id i i
    ) (0 -- 11) in
    let query_values = String.concat ", " rows in
    "insert into board_cards (game_id, idx, card_id) values " ^ query_values ^ ";"

let presence_check pubsub =
    fun test_ctx ->
        let open CCList.Infix in
        let game_id = setup_game pubsub in
        Pubsub.empty_query pubsub (make_board_cards_query game_id);
        let player_id, player_name = setup_player pubsub in
        List.iter (
            fun present -> (
                Pubsub.empty_query pubsub (make_presence_query game_id player_id present);
                let msgs = Pubsub.get_notifications pubsub |> Array.of_list in
                if present then (
                    assert_equal ~printer:string_of_int 2 (Array.length msgs);
                    let expMsg0 = (Messages.make_server_game [
                            Messages.make_player_data player_id player_name present 0 0;
                        ] (make_board_cards_data (0 --^ 12))
                        (make_game_update_data 0 "new" "classic" 3 4 None)) in
                    let expMsg1 = Messages.make_server_presence player_id true in
                    let json0 = msgs.(0).extra in
                    let json1 = msgs.(1).extra in
                    let gotMsg0 = Server_message_converter.of_json json0 in
                    let gotMsg1 = Server_message_converter.of_json json1 in
                    assert_equal ~ctxt:test_ctx expMsg0 gotMsg0 ~printer:Messages.to_string;
                    assert_equal ~ctxt:test_ctx expMsg1 gotMsg1 ~printer:Messages.to_string
                ) else (
                    assert_equal ~printer:string_of_int 1 (Array.length msgs);
                    let expMsg = Messages.make_server_presence player_id false in
                    let json = msgs.(0).extra in
                    let gotMsg = Server_message_converter.of_json json in
                    assert_equal ~ctxt:test_ctx expMsg gotMsg ~printer:Messages.to_string
                )
            )
        ) [true; false];
        teardown_player pubsub player_id;
        teardown_game pubsub game_id

let presence_check_accum pubsub =
    fun test_ctx ->
        let open CCList.Infix in
        let game_id = setup_game pubsub in
        Pubsub.empty_query pubsub (make_board_cards_query game_id);
        let player_id, player_name = setup_player pubsub in
        Pubsub.empty_query pubsub (make_presence_query game_id player_id true);
        Pubsub.empty_query pubsub (make_presence_query game_id player_id false);

        let msgs = Pubsub.get_notifications pubsub in
        assert_equal ~printer:string_of_int 3 (List.length msgs);
        let msgs_arr = Array.of_list msgs in
        let expMsg0 = (Messages.make_server_game [
            Messages.make_player_data player_id player_name true 0 0;
        ] (make_board_cards_data (0 --^ 12))
        (make_game_update_data 0 "new" "classic" 3 4 None)) in
        let expMsg1 = Messages.make_server_presence player_id true in
        let expMsg2 = Messages.make_server_presence player_id false in
        let json0 = msgs_arr.(0).extra in
        let json1 = msgs_arr.(1).extra in
        let json2 = msgs_arr.(2).extra in
        let gotMsg0 = Server_message_converter.of_json json0 in
        let gotMsg1 = Server_message_converter.of_json json1 in
        let gotMsg2 = Server_message_converter.of_json json2 in
        assert_equal ~ctxt:test_ctx expMsg0 gotMsg0 ~printer:Messages.to_string;
        assert_equal ~ctxt:test_ctx expMsg1 gotMsg1 ~printer:Messages.to_string;
        assert_equal ~ctxt:test_ctx expMsg2 gotMsg2 ~printer:Messages.to_string;

        teardown_player pubsub player_id;
        teardown_game pubsub game_id

let player_name_check pubsub =
    fun test_ctx ->
        let game0_id = setup_game pubsub in
        Pubsub.empty_query pubsub (make_board_cards_query game0_id);
        let game1_id = setup_game pubsub in
        Pubsub.empty_query pubsub (make_board_cards_query game1_id);

        let player_id, _ = setup_player pubsub in
        let new_name = "john" in
        Pubsub.empty_query pubsub (make_presence_query game0_id player_id true);
        Pubsub.empty_query pubsub (make_presence_query game1_id player_id true);
        Pubsub.empty_query pubsub @@ Printf.sprintf "update players set name = '%s' where id = %d;" new_name player_id;

        let msgs = Pubsub.get_notifications pubsub in
        assert_equal ~printer:string_of_int 6 (List.length msgs);
        let msgs_arr = Array.of_list msgs in
        let expMsg = Messages.make_server_name player_id new_name in
        let json4 = msgs_arr.(4).extra in
        let json5 = msgs_arr.(5).extra in
        let gotMsg4 = Server_message_converter.of_json json4 in
        let gotMsg5 = Server_message_converter.of_json json5 in
        assert_equal ~ctxt:test_ctx expMsg gotMsg4 ~printer:Messages.to_string;
        assert_equal ~ctxt:test_ctx expMsg gotMsg5 ~printer:Messages.to_string;

        teardown_player pubsub player_id;
        teardown_game pubsub game0_id;
        teardown_game pubsub game1_id

let board_card_check pubsub =
    fun test_ctx ->
        let game_id = setup_game pubsub in
        Pubsub.empty_query pubsub (make_board_cards_query game_id);
        Pubsub.empty_query pubsub @@ Printf.sprintf "update board_cards set card_id = 13 where game_id = %d and idx = 2;" game_id;
        let msgs = Pubsub.get_notifications pubsub in
        assert_equal ~printer:string_of_int 1 (List.length msgs);
        let expMsg = Messages.make_server_board_card 2 13 in
        let gotMsg = (List.hd msgs).extra |> Server_message_converter.of_json in
        assert_equal ~ctxt:test_ctx expMsg gotMsg ~printer:Messages.to_string;
        teardown_game pubsub game_id

let game_update_check pubsub =
    fun test_ctx ->
        let check ?(card_idx=0) ?(status="new") ?(theme="classic") ?(dim0=3) ?(dim1=4) ?(next_game=false) update =
            let game_id = setup_game pubsub in
            let (my_update, next_game_id) = if next_game then
                let ngid = create_game pubsub in
                let u = Printf.sprintf "%s next_game_id = %d" update ngid in
                (u, Some ngid)
            else
                (update, None)
            in
            Pubsub.empty_query pubsub @@ Printf.sprintf "update games set %s where id = %d;" my_update game_id;
            let msgs = Pubsub.get_notifications pubsub in
            assert_equal ~printer:string_of_int 1 (List.length msgs);
            let gotMsg = (List.hd msgs).extra |> Server_message_converter.of_json in
            let expMsg = Messages.make_server_game_update card_idx status theme dim0 dim1 next_game_id in
            assert_equal ~ctxt:test_ctx expMsg gotMsg ~printer:Messages.to_string;
            teardown_game pubsub game_id;
            ()
        in

        check ~card_idx:13 "card_idx = 13";
        check ~status:"started" "status = 'started'";
        check ~status:"complete" "status = 'complete'";
        check ~theme:"open_source" "theme = 'open_source'";
        check ~theme:"hero" "theme = 'hero'";
        check ~dim0:4 "dim0 = 4";
        check ~dim1:3 "dim1 = 3";
        check ~next_game:true "";

        check ~card_idx:13 ~status:"started" "card_idx = 13, status = 'started'";
        check ~status:"started" ~dim0:4 "status = 'started', dim0 = 4";
        check ~status:"complete" ~dim1:3 "status = 'complete', dim1 = 3";
        check ~theme:"open_source" ~status:"complete" "theme = 'open_source', status = 'complete'";
        check ~theme:"open_source" ~dim0:4 "theme = 'open_source', dim0 = 4";
        check ~theme:"open_source" ~dim1:3 "theme = 'open_source', dim1 = 3";
        check ~theme:"hero" ~status:"complete" "theme = 'hero', status = 'complete'";
        check ~theme:"hero" ~dim0:4 "theme = 'hero', dim0 = 4";
        check ~theme:"hero" ~dim1:3 "theme = 'hero', dim1 = 3";
        check ~dim0:4 ~dim1:3 "dim0 = 4, dim1 = 3";
        check ~next_game:true ~card_idx:27 "card_idx = 27,";
        check ~next_game:true ~card_idx:29 ~status:"started" ~theme:"open_source" "card_idx = 29, status = 'started', theme = 'open_source',";
        check ~next_game:true ~card_idx:29 ~status:"started" ~theme:"hero" "card_idx = 29, status = 'started', theme = 'hero',"

let moves_insert_check pubsub =
    fun test_ctx ->
        let game_id = setup_game pubsub in
        let player_id, _ = setup_player pubsub in
        Pubsub.empty_query pubsub @@ Printf.sprintf
            {eos|
                insert into moves (
                    game_id, player_id,
                    idx0, card0_id,
                    idx1, card1_id,
                    idx2, card2_id
                ) values (
                    %d, %d,
                    1, 2,
                    11, 12,
                    21, 22
                )
            |eos} game_id player_id;

        let msgs = Pubsub.get_notifications pubsub in
        assert_equal ~printer:string_of_int 1 (List.length msgs);
        let expMsg = Messages.make_server_move_info (make_score_data player_id 1) (make_move_data (1,2) (11,12) (21,22)) in
        let gotMsg = (List.hd msgs).extra |> Server_message_converter.of_json in
        assert_equal ~ctxt:test_ctx expMsg gotMsg ~printer:Messages.to_string;

        teardown_player pubsub player_id;
        teardown_game pubsub game_id

let shuffles_insert_check pubsub =
    fun test_ctx ->
        let game_id = setup_game pubsub in
        let player_id, _ = setup_player pubsub in
        Pubsub.empty_query pubsub @@ Printf.sprintf
            {eos|
                insert into shuffles (
                    game_id,
                    player_id,
                    sets_on_board
                ) values (
                    %d, %d, 13
                )
            |eos} game_id player_id;

        let msgs = Pubsub.get_notifications pubsub in
        assert_equal ~printer:string_of_int 1 (List.length msgs);
        let expMsg = Messages.make_server_shuffles player_id 13 in
        let gotMsg = (List.hd msgs).extra |> Server_message_converter.of_json in
        assert_equal ~ctxt:test_ctx expMsg gotMsg ~printer:Messages.to_string;

        teardown_player pubsub player_id;
        teardown_game pubsub game_id


(*
 * See bin/test -help
 * bin/test -runner sequential
 * libpq / postgresql library cannot process multiple requests
 * concurrently against a single db connection
 *)
let suite pubsub =
    Crypto.init ();
    let check f = f pubsub in
    cases_of check [
        presence_check;
        presence_check_accum;
        player_name_check;
        board_card_check;
        game_update_check;
        moves_insert_check;
    ]
