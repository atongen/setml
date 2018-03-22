open Lwt.Infix

let (>>=?) m f =
  m >>= (function | Ok x -> f x | Error err -> Lwt.return (Error err))

let create_game_query =
    Caqti_request.find Caqti_type.unit Caqti_type.int
    "insert into games default values returning id"

let create_game (module Db : Caqti_lwt.CONNECTION) =
    Db.find create_game_query () >>=? fun game_id_int ->
    Lwt.return_ok (Util.base36_of_int game_id_int)

let create_player_query =
    Caqti_request.find Caqti_type.unit Caqti_type.int
    "insert into players default values returning id"

let create_player (module Db : Caqti_lwt.CONNECTION) =
    Db.find create_player_query ()

let game_player_present_query =
    Caqti_request.exec Caqti_type.(tup3 int int bool)
    {eos|
    insert into games_players (game_id, player_id, present)
    values (?, ?, ?)
    on conflict (game_id, player_id)
    do update set present = excluded.present;
    |eos}

let game_player_presence (module Db : Caqti_lwt.CONNECTION) game_id player_id present =
    let game_id_int = Util.int_of_base36 game_id in
    Db.exec game_player_present_query (game_id_int, player_id, present)