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
    assert_query_equal db 81 ("select count(*) from game_cards where game_id = " ^ string_of_int game_id) >>= fun () ->
    assert_query_equal db 12 ("select count(*) from board_cards where game_id = " ^ string_of_int game_id) >>= fun () ->
    Lwt.return_unit

let create_player_test db =
  fun () ->
    Db.create_player db () >>=? fun player_id ->
    Db.player_exists db player_id >>=? fun player_exists ->
    Lwt.return (assert_bool "player exists" player_exists)

let game_player_presence_test db =
  fun () ->
    let q s game_id player_id =
      "select " ^ s ^ " " ^
      "from games_players " ^
      "where game_id = " ^ string_of_int game_id ^ " " ^
      "and player_id = " ^ string_of_int player_id
    in
    Db.create_game db () >>=? fun game_id ->
    Db.create_player db () >>=? fun player_id ->
    let s = "select exists(" ^ (q "1" game_id player_id) ^ ")" in
    let p = q "presence" game_id player_id in
    Db.query_bool db s >>=? fun exists_before ->
    refute_bool "no exist before" exists_before;
    Db.game_player_presence db (game_id, player_id, true) >>=? fun () ->
    Db.query_bool db s >>=? fun exists_join ->
    assert_bool "yes exists join" exists_join;
    Db.query_bool db p >>=? fun present_join ->
    assert_bool "yes present join" present_join;
    Db.game_player_presence db (game_id, player_id, false) >>=? fun () ->
    Db.query_bool db s >>=? fun exists_leave ->
    assert_bool "yes exist leave" exists_leave;
    Db.query_bool db p >>=? fun present_leave ->
    refute_bool "no present leave" present_leave;
    Lwt.return_unit

let create_move_test db =
  fun () ->
    Db.create_game db () >>=? fun game_id ->
    Db.create_player db () >>=? fun player_id ->
    Db.game_player_presence db (game_id, player_id, true) >>=? fun () ->
    Db.find_board_cards db game_id >>=? fun board_idxs ->
    let cards = Card.of_int_list board_idxs |> Array.of_list in
    let sets_and_indexes_opt = Card.next_set_and_indexes_of_opt_array cards in
    match sets_and_indexes_opt with
    | Some ((idx0, c0), (idx1, c1), (idx2, c2)) ->
      assert_bool "cards are set" (Card.is_set c0 c1 c2);
      Db.create_move db (game_id, player_id, idx0, c0, idx1, c1, idx2, c2) >>=? fun made_move ->
      assert_bool "made move" made_move;
      Db.find_scoreboard db game_id >>=? fun scores ->
      assert_equal ~printer:string_of_int 1 (List.length scores);
      (match find_player_score scores player_id with
      | Some (score) -> assert_equal ~printer:string_of_int 1 score
      | None -> assert_failure "Player score");
      assert_query_equal db (12+3) (Printf.sprintf "select card_idx from games where id = %d;" game_id) >>= fun () ->
      Db.find_board_cards db game_id >>=? fun new_board_idxs ->
      let board = List.map Card.of_int new_board_idxs |> Array.of_list in
      refute_card_equal c0 board.(idx0);
      refute_card_equal c1 board.(idx1);
      refute_card_equal c2 board.(idx2);
      Lwt.return_unit
    | None -> assert_failure "No sets/indexes found"

let complete_game_test db =
    fun () ->
        Db.create_game db () >>=? fun game_id ->
        Db.create_player db () >>=? fun player_id ->
        Db.game_player_presence db (game_id, player_id, true) >>=? fun () ->
        let rec make_move i shuffled =
            Db.find_game_cards db game_id >>=? fun cards_before ->
            Db.find_board_cards db game_id >>=? fun board_card_ids ->

            if not shuffled then (
                Db.shuffle_board db (game_id, player_id) >>=? fun did_shuffle ->
                if not did_shuffle then (
                    Db.is_game_over db game_id >>=? fun game_over ->
                    if game_over then
                        Lwt.return_unit
                    else
                        assert_failure "game not over"
                ) else (
                    Db.find_game_cards db game_id >>=? fun cards_after ->
                    Db.find_board_cards db game_id >>=? fun board_after ->
                    assert_bool "cards are different after shuffle" (cards_before != cards_after);
                    let card_ids = List.map (fun (_, card_id) -> card_id) cards_after in
                    let scids = List.sort_uniq compare card_ids in
                    assert_equal ~msg:"sorted unique cards should have length 81 after shuffle" ~printer:string_of_int 81 (List.length scids);
                    make_move i true
                )
            ) else (
                let cards = Card.of_int_list board_card_ids |> Array.of_list in
                let sets_and_indexes_opt = Card.next_set_and_indexes_of_opt_array cards in
                match sets_and_indexes_opt with
                | Some ((idx0, c0), (idx1, c1), (idx2, c2)) ->
                    assert_bool "cards are set" (Card.is_set c0 c1 c2);
                    Db.create_move db (game_id, player_id, idx0, c0, idx1, c1, idx2, c2) >>=? fun made_move ->
                    assert_bool "made move" made_move;
                    Db.find_scoreboard db game_id >>=? fun scores ->
                    assert_equal ~msg:"number of players should equal 1" ~printer:string_of_int 1 (List.length scores);
                    (match find_player_score scores player_id with
                    | Some (score) -> assert_equal ~msg:"this player should have made all the moves" ~printer:string_of_int i score
                    | None -> assert_failure "player score");
                    Db.query_int db (Printf.sprintf "select card_idx from games where id = %d;" game_id) >>=? fun card_idx ->
                    assert_equal ~msg:"game card_idx should reflect number of moves" ~printer:string_of_int (12+(3*i)) card_idx;
                    make_move (i+1) false
                | None ->
                    Db.is_game_over db game_id >>=? fun game_over ->
                    if game_over then (
                        Lwt.return_unit
                    ) else (
                        make_move i false
                    )
            )
    in
    make_move 1 false

let create_failed_move_test db =
  fun () ->
    Db.create_game db () >>=? fun game_id ->
    Db.create_player db () >>=? fun player_id ->
    Db.game_player_presence db (game_id, player_id, true) >>=? fun () ->
    Db.find_board_cards db game_id >>=? fun old_board_idxs ->
    let c0, c1, c2 = (Card.of_int 1, Card.of_int 2, Card.of_int 3) in
    Db.create_move db (game_id, player_id, 0, c0, 1, c1, 2, c2) >>=? fun made_move ->
    assert_bool "did not make move" (not made_move);
    Db.find_scoreboard db game_id >>=? fun scores ->
    assert_equal ~printer:string_of_int 1 (List.length scores);
    (match find_player_score scores player_id with
    | Some (score) -> assert_equal ~printer:string_of_int 0 score
    | None -> assert_failure "Player score");
    assert_query_equal db 12 (Printf.sprintf "select card_idx from games where id = %d;" game_id) >>= fun () ->
    Db.find_board_cards db game_id >>=? fun new_board_idxs ->
    assert_bool "board hasn't changed" (old_board_idxs = new_board_idxs);
    Lwt.return_unit
