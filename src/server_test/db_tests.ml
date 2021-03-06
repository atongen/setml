open OUnit2
open Lwt.Infix

open Test_lib.Test_util
open Lib
open Shared

let valid_board_dims = [
    (3, 3);
    (3, 4);
    (4, 3);
    (4, 4);
]

let create_game_test db =
    fun () ->
        let rawDims = Combinatorics.comb0 [2; 2; 3; 3; 4; 4; 5; 5] 2 in
        let allDims = List.map (fun l ->
            let a = Array.of_list l in
            [(a.(0), a.(1)); (a.(1), a.(0))]
        ) rawDims
        |> List.flatten in
        let dims = List.sort_uniq (fun (x0, x1) (y0, y1) ->
            let c0 = compare x0 y0 in
            let c1 = compare x1 y1 in
            if c0 = c1 then c0
            else if c0 = 0 then c1
            else if c1 = 0 then c0
            else c0
        ) allDims in
        List.iter (fun (dim0, dim1) ->
            if dim0 < 3 || dim0 > 4 || dim1 < 3 || dim1 > 4 then (
                ignore (
                    Db.create_game ~dim0 ~dim1 db >>= fun _ ->
                    ignore(assert_failure "invalid game board dimensions");
                    Lwt.return_unit
                );
            ) else (
                ignore (
                    Db.create_game ~dim0 ~dim1 db >>=? fun game_id ->
                    Db.game_exists ~game_id db >>=? fun game_exists ->
                    assert_bool "game exists" game_exists;
                    Db.find_game_cards ~game_id db >>=? fun game_cards ->
                    assert_equal 81 (List.length game_cards);
                    Db.find_board_cards ~game_id db >>=? fun board_cards ->
                    assert_equal (dim0 * dim1) (List.length board_cards);
                    Db.find_game_data ~game_id db >>=? fun game_data ->
                    assert_equal dim0 game_data.dim0;
                    assert_equal dim1 game_data.dim1;
                    Lwt.return_unit
                );
            )
        ) dims;
        Lwt.return_unit

let create_player_test db =
  fun () ->
    Db.create_player db >>=? fun player_id ->
    Db.player_exists ~player_id db >>=? fun player_exists ->
    Lwt.return (assert_bool "player exists" player_exists)

let game_player_presence_test db =
  fun () ->
    Db.create_game db >>=? fun game_id ->
    Db.create_player db >>=? fun player_id ->

    Db.find_game_player_presence ~game_id ~player_id db >>=? fun present_before ->
    refute_bool "no present before" present_before;
    Db.is_player_member ~game_id ~player_id db >>=? fun member_before ->
    refute_bool "no membership before" member_before;

    Db.set_game_player_presence ~game_id ~player_id ~present:true db >>=? fun () ->
    Db.find_game_player_presence ~game_id ~player_id db >>=? fun present_join ->
    assert_bool "yes present join" present_join;
    Db.is_player_member ~game_id ~player_id db >>=? fun member_after_join ->
    assert_bool "yes membership after join" member_after_join;

    Db.set_game_player_presence ~game_id ~player_id ~present:false db >>=? fun () ->
    Db.find_game_player_presence ~game_id ~player_id db >>=? fun present_leave ->
    refute_bool "no present leave" present_leave;
    Db.is_player_member ~game_id ~player_id db >>=? fun member_after_leave ->
    assert_bool "yes membership after leave" member_after_leave;

    Lwt.return_unit

let create_move_test db =
  fun () ->
    let rec aux i dim0 dim1 =
        let board_size = dim0 * dim1 in
        Db.create_game ~dim0 ~dim1 db >>=? fun game_id ->
        Db.create_player db >>=? fun player_id ->
        Db.set_game_player_presence ~game_id ~player_id ~present:true db >>=? fun () ->
        Db.find_board_cards ~game_id db >>=? fun board_cards ->
        if Messages_util.board_cards_exists_set board_cards then (
            match Messages_util.board_cards_next_set board_cards with
            | Some (cd0, cd1, cd2) ->
                Db.create_move ~game_id ~player_id ~cards:(cd0, cd1, cd2) db >>=? fun made_move ->
                assert_equal (board_size + 3) made_move;
                Db.find_player_data ~game_id db >>=? fun players_data ->
                assert_equal ~printer:string_of_int 1 (List.length players_data);
                (match find_player_data players_data player_id with
                | Some (player_data) -> assert_equal ~printer:string_of_int 1 player_data.score
                | None -> assert_failure "Player score");
                Db.find_game_card_idx ~game_id db >>=? fun card_idx ->
                assert_equal (board_size + 3) card_idx;
                Db.find_board_cards ~game_id db >>=? fun new_board_cards ->
                List.iter2 (fun bc_orig bc_new ->
                    assert_bool "board cards have changed" (bc_orig != bc_new)
                ) board_cards new_board_cards;
                Lwt.return_unit
            | None -> assert_failure "No sets/indexes found"
        ) else (
            if i > 0 then
                aux (i - 1) dim0 dim1
            else
                assert_failure "No sets on board after 10 attempts"
        )
    in
    List.iter (fun (dim0, dim1) ->
        ignore(aux 10 dim0 dim1)
    ) valid_board_dims;
    Lwt.return_unit

let complete_game_test db =
    fun () ->
        let aux dim0 dim1 =
            let board_size = dim0 * dim1 in
            Db.create_game ~dim0 ~dim1 db >>=? fun game_id ->
            Db.create_player db >>=? fun player_id ->
            Db.set_game_player_presence ~game_id ~player_id ~present:true db >>=? fun () ->
            let rec make_move i j shuffled =
                Db.find_game_cards ~game_id db >>=? fun cards_before ->
                Db.find_board_cards ~game_id db >>=? fun board_cards ->

                if not shuffled then (
                    Db.create_shuffle ~game_id ~player_id db >>=? fun did_shuffle ->
                    if not did_shuffle then (
                        Db.is_game_over ~game_id db >>=? fun game_over ->
                        if game_over then
                            Lwt.return_unit
                        else
                            (* we couldn't shuffle, but game is not over, there must be sets left on the board *)
                            make_move i j true
                    ) else (
                        Db.find_game_cards ~game_id db >>=? fun cards_after ->
                        Db.find_board_cards ~game_id db >>=? fun _ ->
                        assert_bool "cards are different after shuffle" (cards_before != cards_after);
                        let card_ids = List.map (fun (cd: Messages.card_data) -> Card.to_int cd.card) cards_after in
                        let scids = List.sort_uniq compare card_ids in
                        assert_equal ~msg:"sorted unique cards should have length 81 after shuffle" ~printer:string_of_int 81 (List.length scids);

                        Db.find_player_data ~game_id db >>=? fun players_data ->
                        assert_equal ~msg:"number of players should equal 1" ~printer:string_of_int 1 (List.length players_data);
                        (match find_player_data players_data player_id with
                        | Some (player_data) -> assert_equal ~msg:"this player should have made all the shuffles" ~printer:string_of_int (j+1) player_data.shuffles
                        | None -> assert_failure "player shuffles");

                        make_move i (j+1) true
                    )
                ) else (
                    match Messages_util.board_cards_next_set board_cards with
                    | Some (bc0, bc1, bc2) ->
                        Db.create_move ~game_id ~player_id ~cards:(bc0, bc1, bc2) db >>=? fun made_move ->
                        assert (made_move > board_size);

                        Db.find_player_data ~game_id db >>=? fun players_data ->
                        assert_equal ~msg:"number of players should equal 1" ~printer:string_of_int 1 (List.length players_data);
                        (match find_player_data players_data player_id with
                        | Some (player_data) -> assert_equal ~msg:"this player should have made all the moves" ~printer:string_of_int (i+1) player_data.score
                        | None -> assert_failure "player score");

                        Db.find_game_card_idx ~game_id db >>=? fun card_idx ->
                        (* card_idx cannot exceed 81 *)
                        let exp_card_idx = min 81 (board_size+(3*(i+1))) in
                        assert_equal ~msg:"game card_idx should reflect number of moves" ~printer:string_of_int exp_card_idx card_idx;
                        make_move (i+1) j false
                    | None ->
                        Db.is_game_over ~game_id db >>=? fun game_over ->
                        if game_over then (
                            Lwt.return_unit
                        ) else (
                            make_move i j false
                        )
                ) in
            make_move 0 0 false in
        List.iter (fun (dim0, dim1) ->
            ignore(aux dim0 dim1)
        ) valid_board_dims;
        Lwt.return_unit


let create_failed_move_test db =
    fun () ->
        Db.create_game db >>=? fun game_id ->
        Db.create_player db >>=? fun player_id ->
        Db.set_game_player_presence ~game_id ~player_id ~present:true db >>=? fun () ->
        Db.find_board_cards ~game_id db >>=? fun old_board_idxs ->
        let cd0, cd1, cd2 = (Messages.make_card_data 1 2, Messages.make_card_data 3 4, Messages.make_card_data 5 6) in
        Db.create_move ~game_id ~player_id ~cards:(cd0, cd1, cd2) db >>= function
        | Ok _ -> assert_failure "should not have made move"
        | Error _ ->
            Db.find_player_data ~game_id db >>=? fun players_data ->
            assert_equal ~msg:"" ~printer:string_of_int 1 (List.length players_data);
            (match find_player_data players_data player_id with
            | Some (player_data) -> assert_equal ~printer:string_of_int 0 player_data.score
            | None -> assert_failure "Player score");
            Db.find_game_card_idx ~game_id db >>=? fun card_idx ->
            assert_equal 12 card_idx;
            Db.find_board_cards ~game_id db >>=? fun new_board_idxs ->
            assert_bool "board hasn't changed" (old_board_idxs = new_board_idxs);
            Lwt.return_unit

let game_status_test db =
    fun () ->
        Db.create_game db >>=? fun game_id ->
        Db.find_game_data ~game_id db >>=? fun game_data0 ->
        assert_equal Game_status.New game_data0.status;
        Db.start_game ~game_id db >>=? fun () ->
        Db.find_game_data ~game_id db >>=? fun game_data1 ->
        assert_equal Game_status.Started game_data1.status;
        Db.end_game ~game_id db >>=? fun () ->
        Db.find_game_data ~game_id db >>=? fun game_data2 ->
        assert_equal Game_status.Complete game_data2.status;
        Lwt.return_unit

let is_game_active_test db =
    fun () ->
        Db.is_game_active ~game_id:232424238948 db >>=? fun active ->
        refute_bool "non-existing game is not active" active;

        Db.create_game db >>=? fun game_id ->
        Db.is_game_active ~game_id db >>=? fun active ->
        assert_bool "new game is active" active;

        Db.start_game ~game_id db >>=? fun () ->
        Db.is_game_active ~game_id db >>=? fun active ->
        assert_bool "started game is active" active;

        Db.end_game ~game_id db >>=? fun () ->
        Db.is_game_active ~game_id db >>=? fun active ->
        refute_bool "completed game is not active" active;
        Lwt.return_unit

let can_join_test db =
    fun () ->
        let fake_game_id = 345623452362345 in
        Db.create_game db >>=? fun game_id ->
        Db.create_player db >>=? fun player0_id -> (* present membership *)
        Db.set_game_player_presence ~game_id ~player_id:player0_id ~present:true db >>=? fun () ->
        Db.create_player db >>=? fun player1_id -> (* not-present membership *)
        Db.set_game_player_presence ~game_id ~player_id:player1_id ~present:false db >>=? fun () ->
        Db.create_player db >>=? fun player2_id -> (* no membership *)

        (* non-existing game *)
        Db.can_join ~game_id:fake_game_id ~player_id:player0_id db >>=? fun p0 ->
        refute_bool "present player cannot join non-existing game" p0;

        Db.can_join ~game_id:fake_game_id ~player_id:player1_id db >>=? fun p1 ->
        refute_bool "not-present player cannot join non-existing game" p1;

        Db.can_join ~game_id:fake_game_id ~player_id:player2_id db >>=? fun p2 ->
        refute_bool "non-member player cannot join non-existing game" p2;

        (* new game *)
        Db.can_join ~game_id ~player_id:player0_id db >>=? fun n0 ->
        assert_bool "present player can join new game" n0;

        Db.can_join ~game_id ~player_id:player1_id db >>=? fun n1 ->
        assert_bool "non-present player can join new game" n1;

        Db.can_join ~game_id ~player_id:player2_id db >>=? fun n2 ->
        assert_bool "non-member player can join new game" n2;

        (* started game *)
        Db.start_game ~game_id db >>=? fun () ->
        Db.can_join ~game_id ~player_id:player0_id db >>=? fun s0 ->
        assert_bool "present player can join started game" s0;

        Db.can_join ~game_id ~player_id:player1_id db >>=? fun s1 ->
        assert_bool "non-present player can join started game" s1;

        Db.can_join ~game_id ~player_id:player2_id db >>=? fun s2 ->
        assert_bool "non-member player can join started game" s2;

        (* completed game *)
        Db.end_game ~game_id db >>=? fun () ->
        Db.can_join ~game_id ~player_id:player0_id db >>=? fun c0 ->
        assert_bool "present player can join completed game" c0;

        Db.can_join ~game_id ~player_id:player1_id db >>=? fun c1 ->
        assert_bool "non-present player can join completed game" c1;

        Db.can_join ~game_id ~player_id:player2_id db >>=? fun c2 ->
        refute_bool "non-member player cannot join completed game" c2;

        Lwt.return_unit

let update_player_name_test db =
    fun () ->
        let new_name = "Michael Scott" in
        Db.create_player db >>=? fun player_id ->
        Db.get_player_name ~player_id db >>=? fun player_name_before ->
        assert_equal "" player_name_before;
        Db.update_player_name ~player_id ~name:new_name db >>=? fun () ->
        Db.get_player_name ~player_id db >>=? fun player_name_after ->
        assert_equal new_name player_name_after;
        Lwt.return_unit

let update_game_theme_test db =
    fun () ->
        Db.create_game db >>=? fun game_id ->
        Db.find_game_data ~game_id db >>=? fun game_data_before ->
        assert_equal Theme.Classic game_data_before.theme; (* classic is default *)

        Db.update_game_theme ~game_id ~theme:Theme.Open_source db >>=? fun () ->
        Db.find_game_data ~game_id db >>=? fun game_data_after ->
        assert_equal Theme.Open_source game_data_after.theme;

        Db.update_game_theme ~game_id ~theme:Theme.Classic db >>=? fun () ->
        Db.find_game_data ~game_id db >>=? fun game_data_after2 ->
        assert_equal Theme.Classic game_data_after2.theme;

        Db.update_game_theme ~game_id ~theme:Theme.Hero db >>=? fun () ->
        Db.find_game_data ~game_id db >>=? fun game_data_after3 ->
        assert_equal Theme.Hero game_data_after3.theme;

        Lwt.return_unit

let player_games_test db =
    let assert_pg_equal game_data_list player_games_list =
        assert_equal ~msg:"player_games length" ~printer:string_of_int (List.length game_data_list) (List.length player_games_list);
        List.iter2 (fun (game_id, (game_data: Messages.game_update_data)) (player_game: Api_messages.player_game) ->
            assert_equal ~msg:"player_games game_id" game_id player_game.id;
            assert_equal ~msg:"player_games status" game_data.status player_game.status;
            assert_equal ~msg:"player_games card_idx" game_data.card_idx player_game.card_idx;
        ) game_data_list player_games_list;
    in
    fun () ->
        Db.create_player db >>=? fun player_id ->
        Db.find_player_games ~player_id db >>=? fun player_games0 ->
        assert_equal ~msg:"empty player games up" 0 (List.length player_games0);

        Db.create_game db >>=? fun game1_id ->
        Db.find_game_data ~game_id:game1_id db >>=? fun game_data1 ->
        Db.set_game_player_presence ~game_id:game1_id ~player_id ~present:true db >>=? fun () ->
        Db.find_player_games ~player_id db >>=? fun player_games1 ->
        assert_pg_equal [(game1_id, game_data1)] player_games1;

        Db.create_game db >>=? fun game2_id ->
        Db.start_game ~game_id:game2_id db >>=? fun () ->
        Db.find_game_data ~game_id:game2_id db >>=? fun game_data2 ->
        Db.set_game_player_presence ~game_id:game2_id ~player_id ~present:true db >>=? fun () ->
        Db.find_player_games ~player_id db >>=? fun player_games2 ->
        assert_pg_equal [
            (game2_id, game_data2);
            (game1_id, game_data1);
        ] player_games2;

        Db.create_game db >>=? fun game3_id ->
        Db.start_game ~game_id:game3_id db >>=? fun () ->
        Db.end_game ~game_id:game3_id db >>=? fun () ->
        Db.find_game_data ~game_id:game3_id db >>=? fun game_data3 ->
        Db.set_game_player_presence ~game_id:game3_id ~player_id ~present:true db >>=? fun () ->
        Db.find_player_games ~player_id db >>=? fun player_games3 ->
        assert_pg_equal [
            (game3_id, game_data3);
            (game2_id, game_data2);
            (game1_id, game_data1);
        ] player_games3;

        Lwt.return_unit

let create_game_from_previous_test db =
    fun () ->
        let t (dim0, dim1) theme =
            Db.create_game ~dim0 ~dim1 db >>=? fun game0_id ->
            Db.update_game_theme ~game_id:game0_id ~theme db >>=? fun () ->

            (* first in chain *)
            Db.create_game_from_previous ~game_id:game0_id db >>=? fun game1_id ->
            Db.create_game_from_previous ~game_id:game0_id db >>=? fun game2_id ->
            Db.create_game_from_previous ~game_id:game0_id db >>=? fun game3_id ->
            assert_equal game1_id game2_id;
            assert_equal game2_id game3_id;

            Db.find_game_data ~game_id:game0_id db >>=? fun game0_data ->
            Db.find_game_data ~game_id:game1_id db >>=? fun game1_data ->

            assert_equal game0_data.dim0 dim0;
            assert_equal game0_data.dim1 dim1;
            assert_equal game0_data.theme theme;

            assert_equal game1_data.dim0 dim0;
            assert_equal game1_data.dim1 dim1;
            assert_equal game1_data.theme theme;

            assert_equal (Some game1_id) game0_data.next_game_id;
            assert_equal None game1_data.next_game_id;

            (* second in chain *)
            Db.create_game_from_previous ~game_id:game1_id db >>=? fun game4_id ->
            Db.create_game_from_previous ~game_id:game1_id db >>=? fun game5_id ->
            Db.create_game_from_previous ~game_id:game1_id db >>=? fun game6_id ->
            assert_equal game4_id game5_id;
            assert_equal game5_id game6_id;

            Db.find_game_data ~game_id:game1_id db >>=? fun game1_data_after ->
            Db.find_game_data ~game_id:game4_id db >>=? fun game4_data ->

            assert_equal game1_data_after.dim0 dim0;
            assert_equal game1_data_after.dim1 dim1;
            assert_equal game1_data_after.theme theme;

            assert_equal game4_data.dim0 dim0;
            assert_equal game4_data.dim1 dim1;
            assert_equal game4_data.theme theme;

            assert_equal (Some game4_id) game1_data_after.next_game_id;
            assert_equal None game4_data.next_game_id;

            (* invalid previous game *)
            Db.create_game_from_previous ~game_id:1234567890 db >>= function
            | Ok _game_id -> assert_failure "Created game from invalid previous game"
            | Error _e -> Lwt.return_unit
        in
        List.iter (fun dims ->
            List.iter (fun theme ->
                ignore(t dims theme);
                ()
            ) [Theme.Classic; Theme.Open_source; Theme.Hero];
        ) valid_board_dims;

        Lwt.return_unit

(* don't run this every time *)
let create_many_games_test db =
    fun () ->
        let rec aux i n r =
            Db.create_game db >>=? fun game_id ->
            if i mod r = 0 then begin
                let b = Base_conv.base36_of_int game_id in
                ignore(
                    Lwt_io.eprintf "i=%d game_id=%d b36=%s\n" i game_id b >>= fun () ->
                    Lwt.return_unit
                );
            end;
            if i < n then aux (i + 1) n r
            else Lwt.return_unit
        in
        aux 0 50000 100
