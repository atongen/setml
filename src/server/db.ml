let create_game_query =
    Caqti_request.find_opt Caqti_type.unit Caqti_type.int "insert into games default values returning id"

let create_game (module Db : Caqti_lwt.CONNECTION) =
    Db.find_opt create_game_query ()

let create_player_query =
    Caqti_request.find_opt Caqti_type.unit Caqti_type.int "insert into players default values returning id"

let create_player (module Db : Caqti_lwt.CONNECTION) =
    Db.find_opt create_player_query ()