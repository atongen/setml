open Lwt.Infix

open Test_lib.Test_util
open Lib

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
        "complete_game_test", Db_tests.complete_game_test db;
        "create_failed_move_test", Db_tests.create_failed_move_test db;
        "game_status_test", Db_tests.game_status_test db;
        "update_player_name_test", Db_tests.update_player_name_test db;
        "update_game_theme_test", Db_tests.update_game_theme_test db;
        "player_games_test", Db_tests.player_games_test db;
        "create_game_from_previous", Db_tests.create_game_from_previous_test db;
    ]

let () =
    Crypto.init ();
    let config = Config.make_of_env () in
    Lwt_main.run (begin
        Db.make ~max_size:config.db_pool (Config.db_uri_str config) >>=? fun db ->
        Db.delete_all db () >>=? fun () ->
        suite db >|= OUnit2.run_test_tt_main
    end)
