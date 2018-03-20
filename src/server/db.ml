let create_game_query =
    Caqti_request.find_opt Caqti_type.unit Caqti_type.int "insert into games default values returning id"

let create_game (module Db : Caqti_lwt.CONNECTION) =
    Db.find_opt create_game_query