open Lib
open Lwt
open Lwt.Infix
open OUnit2

open Shared

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

let refute_bool msg got = assert_bool msg (not got)

let create_game_test db =
    fun () ->
        Db.create_game db >>=? fun game_id ->
        Db.game_exists db game_id >>=? fun game_exists ->
        assert_bool "game exists" game_exists;
        assert_query_equal db 81 ("select count(*) from game_cards where game_id = " ^ string_of_int game_id) >>= fun () ->
        assert_query_equal db 12 ("select count(*) from board_cards where game_id = " ^ string_of_int game_id) >>= fun () ->
        Lwt.return_unit

let create_player_test db =
    fun () ->
        Db.create_player db >>=? fun player_id ->
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
        Db.create_game db >>=? fun game_id ->
        Db.create_player db >>=? fun player_id ->
        let s = "select exists(" ^ (q "1" game_id player_id) ^ ")" in
        let p = q "present" game_id player_id in
        Db.query_bool db s >>=? fun exists_before ->
        refute_bool "no exist before" exists_before;
        Db.game_player_presence db game_id player_id true >>=? fun () ->
        Db.query_bool db s >>=? fun exists_join ->
        assert_bool "yes exists join" exists_join;
        Db.query_bool db p >>=? fun present_join ->
        assert_bool "yes present join" present_join;
        Db.game_player_presence db game_id player_id false >>=? fun () ->
        Db.query_bool db s >>=? fun exists_leave ->
        assert_bool "yes exist leave" exists_leave;
        Db.query_bool db p >>=? fun present_leave ->
        refute_bool "no present leave" present_leave;
        Lwt.return_unit

let create_move_test db =
    fun () ->
        Db.create_game db >>=? fun game_id ->
        Db.create_player db >>=? fun player_id ->
        Db.find_board_cards db game_id >>=? fun card_idxs ->
        let cards = List.map Card.of_int card_idxs in
        let sets_and_indexes_opt = Card.next_set_and_indexes cards in
        match sets_and_indexes_opt with
        | Some ((idx0, c0), (idx1, c1), (idx2, c2)) ->
            let c0id = Card.to_int c0 in
            let c1id = Card.to_int c1 in
            let c2id = Card.to_int c2 in
            Db.create_move db game_id player_id idx0 c0id idx1 c1id idx2 c2id >>=? fun () ->
            Db.find_player_score db game_id >>=? fun l ->
            assert_equal ~printer:string_of_int 1 (List.length l);
            Lwt.return_unit
        | None -> assert_failure "No sets/indexes found"

let ats db =
    do_async_tests [
        "create_game_test", create_game_test db;
        "create_player_test", create_player_test db;
        "game_player_presence_test", game_player_presence_test db;
        "create_move_test", create_move_test db;
    ]

let _ = Lwt_main.run (begin
    Caqti_lwt.connect (Uri.of_string "postgresql://atongen:at1234@localhost:5435/setml_test") >>=? fun db ->
    Db.delete_all db >>=? function () ->
    ats db >|= OUnit2.run_test_tt_main
end)
