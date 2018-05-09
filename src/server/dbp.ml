open Lwt.Infix
open Shared

let with_pool_0 pool f =
    Caqti_lwt.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
        f (module C : Caqti_lwt.CONNECTION)
    ) pool

let with_pool_1 pool f arg =
    Caqti_lwt.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
        f (module C : Caqti_lwt.CONNECTION) arg
    ) pool

let query_int pool q = with_pool_1 pool Db.query_int q

let query_bool pool q = with_pool_1 pool Db.query_bool q

let create_game pool = with_pool_0 pool Db.create_game

let game_exists pool game_id = with_pool_1 pool Db.game_exists game_id

let create_player pool = with_pool_0 pool Db.create_player

let player_exists pool player_id = with_pool_1 pool Db.player_exists player_id

let game_player_presence pool args = with_pool_1 pool Db.game_player_presence args

let find_board_cards pool game_id = with_pool_1 pool Db.find_board_cards game_id

let create_move pool args = with_pool_1 pool Db.create_move args

let find_scoreboard pool game_id = with_pool_1 pool Db.find_scoreboard game_id

let update_player_name pool args = with_pool_1 pool Db.update_player_name args

let delete_all pool = with_pool_0 pool Db.delete_all
