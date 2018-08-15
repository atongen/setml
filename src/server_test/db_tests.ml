open OUnit2
open Lwt
open Lwt.Infix

open Test_lib.Test_util
open Lib
open Shared

let create_game_test db =
    fun () ->
        ignore(
            List.iter (fun (dim0, dim1) ->
                Db.create_game db (dim0, dim1) >>=? fun game_id ->
                Db.game_exists db game_id >>=? fun game_exists ->
                assert_bool "game exists" game_exists;
                Db.find_game_cards db (game_id, 0) >>=? fun game_cards ->
                assert_equal 81 (List.length game_cards);
                Db.find_board_cards db game_id >>=? fun board_cards ->
                assert_equal (dim0 * dim1) (List.length board_cards);
                Db.find_game_data db game_id >>=? fun game_data ->
                assert_equal dim0 game_data.dim0;
                assert_equal dim1 game_data.dim1;
            ) [(3, 3); (3, 4); (4, 3); (4, 4)]
        );
        Lwt.return_unit

let create_player_test db =
  fun () ->
    Db.create_player db () >>=? fun player_id ->
    Db.player_exists db player_id >>=? fun player_exists ->
    Lwt.return (assert_bool "player exists" player_exists)

let game_player_presence_test db =
  fun () ->
    Db.create_game db (3, 4) >>=? fun game_id ->
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

let create_move_test db =
  fun () ->
    let rec aux i =
        Db.create_game db (3, 4) >>=? fun game_id ->
        Db.create_player db () >>=? fun player_id ->
        Db.set_game_player_presence db (game_id, player_id, true) >>=? fun () ->
        Db.find_board_cards db game_id >>=? fun board_cards ->
        if Messages_util.board_cards_exists_set board_cards then (
            match Messages_util.board_cards_next_set board_cards with
            | Some (cd0, cd1, cd2) ->
                Db.create_move db (game_id, player_id, (cd0, cd1, cd2)) >>=? fun made_move ->
                assert_equal 15 made_move;
                Db.find_player_data db game_id >>=? fun players_data ->
                assert_equal ~printer:string_of_int 1 (List.length players_data);
                (match find_player_data players_data player_id with
                | Some (player_data) -> assert_equal ~printer:string_of_int 1 player_data.score
                | None -> assert_failure "Player score");
                Db.find_game_card_idx db game_id >>=? fun card_idx ->
                assert_equal (12+3) card_idx;
                Db.find_board_cards db game_id >>=? fun new_board_cards ->
                List.iter2 (fun bc_orig bc_new ->
                    assert_bool "board cards have changed" (bc_orig != bc_new)
                ) board_cards new_board_cards;
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
        Db.create_game db (3, 4) >>=? fun game_id ->
        Db.create_player db () >>=? fun player_id ->
        Db.set_game_player_presence db (game_id, player_id, true) >>=? fun () ->
        let rec make_move i j shuffled =
            Db.find_game_cards db (game_id, 0) >>=? fun cards_before ->
            Db.find_board_cards db game_id >>=? fun board_cards ->

            if not shuffled then (
                Db.create_shuffle db (game_id, player_id) >>=? fun did_shuffle ->
                if not did_shuffle then (
                    Db.is_game_over db game_id >>=? fun game_over ->
                    if game_over then
                        Lwt.return_unit
                    else
                        (* we couldn't shuffle, but game is not over, there must be sets left on the board *)
                        make_move i j true
                ) else (
                    Db.find_game_cards db (game_id, 0) >>=? fun cards_after ->
                    Db.find_board_cards db game_id >>=? fun board_after ->
                    assert_bool "cards are different after shuffle" (cards_before != cards_after);
                    let card_ids = List.map (fun (cd: Messages.card_data) -> Card.to_int cd.card) cards_after in
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
                match Messages_util.board_cards_next_set board_cards with
                | Some (bc0, bc1, bc2) ->
                    Db.create_move db (game_id, player_id, (bc0, bc1, bc2)) >>=? fun made_move ->
                    assert (made_move > 12);

                    Db.find_player_data db game_id >>=? fun players_data ->
                    assert_equal ~msg:"number of players should equal 1" ~printer:string_of_int 1 (List.length players_data);
                    (match find_player_data players_data player_id with
                    | Some (player_data) -> assert_equal ~msg:"this player should have made all the moves" ~printer:string_of_int (i+1) player_data.score
                    | None -> assert_failure "player score");

                    Db.find_game_card_idx db game_id >>=? fun card_idx ->
                    (* card_idx cannot exceed 81 *)
                    let exp_card_idx = min 81 (12+(3*(i+1))) in
                    assert_equal ~msg:"game card_idx should reflect number of moves" ~printer:string_of_int exp_card_idx card_idx;
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
        Db.create_game db (3, 4) >>=? fun game_id ->
        Db.create_player db () >>=? fun player_id ->
        Db.set_game_player_presence db (game_id, player_id, true) >>=? fun () ->
        Db.find_board_cards db game_id >>=? fun old_board_idxs ->
        let cd0, cd1, cd2 = (Messages.make_card_data 1 2, Messages.make_card_data 3 4, Messages.make_card_data 5 6) in
        Db.create_move db (game_id, player_id, (cd0, cd1, cd2)) >>= function
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

let game_status_test db =
    fun () ->
        Db.create_game db (3, 4) >>=? fun game_id ->
        Db.start_game db game_id >>=? fun () ->
        Db.end_game db game_id >>=? fun () ->
        Lwt.return_unit

(* TODO: Add tests with game board != 3x4 *)
