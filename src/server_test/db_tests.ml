open OUnit2
open Lwt
open Lwt.Infix

open Test_lib.Test_util
open Lib
open Shared

let create_game_test db =
  fun () ->
    Db.create_game db () >>=? fun game_id ->
    Db.game_exists db game_id >>=? fun game_exists ->
    assert_bool "game exists" game_exists;
    Db.find_game_cards db (game_id, 0) >>=? fun game_cards ->
    assert_equal 81 (List.length game_cards);
    Db.find_board_cards db game_id >>=? fun board_cards ->
    assert_equal 12 (List.length board_cards);
    Lwt.return_unit

let create_player_test db =
  fun () ->
    Db.create_player db () >>=? fun player_id ->
    Db.player_exists db player_id >>=? fun player_exists ->
    Lwt.return (assert_bool "player exists" player_exists)

let game_player_presence_test db =
  fun () ->
    Db.create_game db () >>=? fun game_id ->
    Db.create_player db () >>=? fun player_id ->

    Db.find_game_player_presence db (game_id, player_id) >>=? fun present_before ->
    refute_bool "no present before" present_before;

    Db.set_game_player_presence db (game_id, player_id, true) >>=? fun () ->
    Db.find_game_player_presence db (game_id, player_id) >>=? fun present_join ->
    assert_bool "yes present join" present_join;

    Db.set_game_player_presence db (game_id, player_id, false) >>=? fun () ->
    Db.find_game_player_presence db (game_id, player_id) >>=? fun present_leave ->
    refute_bool "no present leave" present_leave;

    Lwt.return_unit

(* HERE *)
let create_move_test db =
  fun () ->
    let rec aux i =
        Db.create_game db () >>=? fun game_id ->
        Db.create_player db () >>=? fun player_id ->
        Db.set_game_player_presence db (game_id, player_id, true) >>=? fun () ->
        Db.find_board_cards db game_id >>=? fun board_cards ->
        if Card.exists_set cc then (
            let sets_and_indexes_opt = Card.next_set_and_indexes_of_opt_array (Array.of_list cards) in
            match sets_and_indexes_opt with
            | Some ((idx0, c0), (idx1, c1), (idx2, c2)) ->
                assert_bool "cards are set" (Card.is_set c0 c1 c2);
                Db.create_move db (game_id, player_id, ((idx0, c0), (idx1, c1), (idx2, c2))) >>=? fun made_move ->
                assert_equal 15 made_move;
                Db.find_player_data db game_id >>=? fun players_data ->
                assert_equal ~printer:string_of_int 1 (List.length players_data);
                (match find_player_data players_data player_id with
                | Some (player_data) -> assert_equal ~printer:string_of_int 1 player_data.score
                | None -> assert_failure "Player score");
                Db.find_game_card_idx db game_id >>=? fun card_idx ->
                assert_equal (12+3) card_idx;
                Db.find_board_cards db game_id >>=? fun board_cards ->
                let board = board_cards |> Array.of_list in
                List.iter (fun (idx, c_orig) ->
                    match board.(idx) with
                    | Some c -> refute_card_equal c_orig c
                    | None -> assert_failure "board card not found"
                ) [(idx0, c0); (idx1, c1); (idx2, c2)];
                Lwt.return_unit
            | None -> assert_failure "No sets/indexes found"
        ) else (
            if i > 0 then
                aux (i - 1)
            else
                assert_failure "No sets on board after 10 attempts"
        )
    in
    aux 10

let complete_game_test db =
    fun () ->
        Db.create_game db () >>=? fun game_id ->
        Db.create_player db () >>=? fun player_id ->
        Db.set_game_player_presence db (game_id, player_id, true) >>=? fun () ->
        let rec make_move i j shuffled =
            Db.find_game_cards db (game_id, 0) >>=? fun cards_before ->
            Db.find_board_cards db game_id >>=? fun board_cards ->

            if not shuffled then (
                Db.shuffle_board db (game_id, player_id) >>=? fun did_shuffle ->
                if not did_shuffle then (
                    Db.is_game_over db game_id >>=? fun game_over ->
                    if game_over then
                        Lwt.return_unit
                    else
                        assert_failure "game not over"
                ) else (
                    Db.find_game_cards db (game_id, 0) >>=? fun cards_after ->
                    Db.find_board_cards db game_id >>=? fun board_after ->
                    assert_bool "cards are different after shuffle" (cards_before != cards_after);
                    let card_ids = List.map (fun (_, card_id) -> card_id) cards_after in
                    let scids = List.sort_uniq compare card_ids in
                    assert_equal ~msg:"sorted unique cards should have length 81 after shuffle" ~printer:string_of_int 81 (List.length scids);

                    Db.find_player_data db game_id >>=? fun players_data ->
                    assert_equal ~msg:"number of players should equal 1" ~printer:string_of_int 1 (List.length players_data);
                    (match find_player_data players_data player_id with
                    | Some (player_data) -> assert_equal ~msg:"this player should have made all the shuffles" ~printer:string_of_int (j+1) player_data.shuffles
                    | None -> assert_failure "player shuffles");

                    make_move i (j+1) true
                )
            ) else (
                let cards = Array.of_list board_cards in
                let sets_and_indexes_opt = Card.next_set_and_indexes_of_opt_array cards in
                match sets_and_indexes_opt with
                | Some ((idx0, c0), (idx1, c1), (idx2, c2)) ->
                    assert_bool "cards are set" (Card.is_set c0 c1 c2);
                    Db.create_move db (game_id, player_id, ((idx0, c0), (idx1, c1), (idx2, c2))) >>=? fun made_move ->
                    assert (made_move > 12);

                    Db.find_player_data db game_id >>=? fun players_data ->
                    assert_equal ~msg:"number of players should equal 1" ~printer:string_of_int 1 (List.length players_data);
                    (match find_player_data players_data player_id with
                    | Some (player_data) -> assert_equal ~msg:"this player should have made all the moves" ~printer:string_of_int (i+1) player_data.score
                    | None -> assert_failure "player score");

                    Db.find_game_card_idx db  game_id >>=? fun card_idx ->
                    assert_equal ~msg:"game card_idx should reflect number of moves" ~printer:string_of_int (12+(3*(i+1))) card_idx;
                    make_move (i+1) j false
                | None ->
                    Db.is_game_over db game_id >>=? fun game_over ->
                    if game_over then (
                        Lwt.return_unit
                    ) else (
                        make_move i j false
                    )
            )
    in
    make_move 0 0 false

let create_failed_move_test db =
    fun () ->
        Db.create_game db () >>=? fun game_id ->
        Db.create_player db () >>=? fun player_id ->
        Db.set_game_player_presence db (game_id, player_id, true) >>=? fun () ->
        Db.find_board_cards db game_id >>=? fun old_board_idxs ->
        let c0, c1, c2 = (Card.of_int 1, Card.of_int 2, Card.of_int 3) in
        Db.create_move db (game_id, player_id, ((0, c0), (1, c1), (2, c2))) >>= function
        | Ok _ -> assert_failure "should not have made move"
        | Error e ->
            Db.find_player_data db game_id >>=? fun players_data ->
            assert_equal ~printer:string_of_int 1 (List.length players_data);
            (match find_player_data players_data player_id with
            | Some (player_data) -> assert_equal ~printer:string_of_int 0 player_data.score
            | None -> assert_failure "Player score");
            Db.find_game_card_idx db game_id >>=? fun card_idx ->
            assert_equal 12 card_idx;
            Db.find_board_cards db game_id >>=? fun new_board_idxs ->
            assert_bool "board hasn't changed" (old_board_idxs = new_board_idxs);
            Lwt.return_unit
