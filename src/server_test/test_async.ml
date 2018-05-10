open OUnit2
open Lwt
open Lwt.Infix

open Test_lib.Test_util
open Lib
open Shared

(*
 * OUnit2 test suite of tests for asynchronous libraries using Lwt
 * Should be safe to run in parallel, ie bin/test -runner processes
 *)

let suite db =
    do_async_tests [
        "create_game_test", Db_tests.create_game_test db;
        "create_player_test", Db_tests.create_player_test db;
        "game_player_presence_test", Db_tests.game_player_presence_test db;
        "create_move_test", Db_tests.create_move_test db;
        "create_failed_move_test", Db_tests.create_failed_move_test db;
    ]

let pool_suite pool =
    do_async_tests [
        "create_game_test_pool", Dbp_tests.create_game_test pool;
        "create_player_test_pool", Dbp_tests.create_player_test pool;
        "game_player_presence_test_pool", Dbp_tests.game_player_presence_test pool;
        "create_move_test_pool", Dbp_tests.create_move_test pool;
        "create_failed_move_test_pool", Dbp_tests.create_failed_move_test pool;
    ]

let run_db_suite () =
    Lwt_main.run (begin
        Caqti_lwt.connect (Uri.of_string "postgresql://atongen:at1234@localhost:5435/setml_test") >>=? fun db ->
        Db.delete_all db () >>=? fun () ->
        suite db >|= OUnit2.run_test_tt_main
    end)

let run_pool_suite () =
    Lwt_main.run (begin
        Lwt.return (Caqti_lwt.connect_pool ~max_size:5 (Uri.of_string "postgresql://atongen:at1234@localhost:5435/setml_test")) >>=? fun pool ->
        Dbp.delete_all pool () >>=? fun () ->
        pool_suite pool >|= OUnit2.run_test_tt_main
    end)

let _ = run_db_suite (); run_pool_suite ()
