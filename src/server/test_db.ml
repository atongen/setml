open Lib
open Lwt
open Lwt.Infix
open OUnit2

let (>>=?) m f =
    m >>= function
        | Ok x -> f x
        | Error (#Caqti_error.t as err) -> Lwt.fail (Caqti_error.Exn err)

let do_async_tests ?(name="lwt test") tests =
    let results =
      tests
      |> Lwt_list.map_s (fun (name, test) ->
        Logs.info (fun f -> f "Running %s" name);
        let res = Lwt.try_bind test
                    (fun () -> return `Ok)
                    (fun exn -> return (`Exn exn)) in
        res >|= (fun res -> (name, res))) in
    results >|= (fun results ->
      let ounit_tests =
        results
        |> List.map (fun (name, res) ->
          name >:: fun ctx ->
            match res with
            | `Ok -> ()
            | `Exn x -> raise x) in
          name >::: ounit_tests)

let assert_query_equal db exp q =
    Db.query_int db q >>=?  fun got ->
    assert_equal ~printer:string_of_int exp got;
    Lwt.return_unit

let create_game_test db =
    fun () ->
        Db.create_game db >>=? function game_id ->
        Db.game_exists db game_id >>=? fun game_exists ->
        assert_bool "game exists" game_exists;
        assert_query_equal db 81 ("select count(*) from game_cards where game_id = " ^ string_of_int game_id) >>= fun () ->
        assert_query_equal db 12 ("select count(*) from board_cards where game_id = " ^ string_of_int game_id) >>= fun () ->
        Lwt.return_unit

let create_player_test db =
    fun () ->
        Db.create_player db >>=? function player_id ->
        Db.player_exists db player_id >>=? fun player_exists ->
        Lwt.return (assert_bool "player exists" player_exists)

let ats db =
    do_async_tests [
        "create_game_test", create_game_test db;
        "create_player_test", create_player_test db
    ]

let _ = Lwt_main.run (begin
    Caqti_lwt.connect (Uri.of_string "postgresql://atongen:at1234@localhost:5435/setml_test") >>=? fun db ->
    Db.delete_all db >>=? function () ->
    ats db >|= OUnit2.run_test_tt_main
end)
