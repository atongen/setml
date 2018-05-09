open OUnit2
open Lwt
open Lwt.Infix

open Test_lib.Test_util
open Lib
open Shared

let create_game_test pool =
  fun () ->
    Dbp.create_game pool >>=? fun game_id ->
    Dbp.game_exists pool game_id >>=? fun game_exists ->
    assert_bool "game exists" game_exists;
    assert_pool_query_equal pool 81 ("select count(*) from game_cards where game_id = " ^ string_of_int game_id) >>= fun () ->
    assert_pool_query_equal pool 12 ("select count(*) from board_cards where game_id = " ^ string_of_int game_id) >>= fun () ->
    Lwt.return_unit
