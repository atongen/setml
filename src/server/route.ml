type t =
    | Index
    | Ws_show of int
    | Game_create
    | Game_show of int
    | Static
    | Route_not_found

let string_of_game_id id = Util.base36_of_int id
let game_id_string str = Util.int_of_base36 str

let path_parts path =
    Str.split (Str.regexp "\/+") path |> Array.of_list

let of_meth_and_path meth path =
    let parts = path_parts path in
    let n = Array.length parts in
    if n == 0 && meth == `GET then
        Index
    else if n == 1 && String.equal parts.(0) "games" && meth == `POST then
        Game_create
    else if n == 2 && String.equal parts.(0) "games" && meth == `GET then
        let game_id = game_id_string parts.(1) in
        Game_show game_id
    else if n == 3 && String.equal parts.(0) "games" && String.equal parts.(2) "ws" && meth == `GET then
        let game_id = game_id_string parts.(1) in
        Ws_show game_id
    else if meth == `GET then
        Static
    else
        Route_not_found

let of_req req =
    let meth = Cohttp.Request.meth req in
    let uri = Cohttp.Request.uri req in
    let path = Uri.path uri in
    of_meth_and_path meth path

let to_meth_and_path = function
    | Index -> (`GET, "/")
    | Ws_show (game_id) ->
        let game_id_str = string_of_game_id game_id in
        (`GET, "/games/" ^ game_id_str ^ "/ws")
    | Game_create -> (`POST, "/games")
    | Game_show (game_id) ->
        let game_id_str = string_of_game_id game_id in
        (`GET, "/games/" ^ game_id_str)
    | Static -> failwith "Cannot build static route"
    | Route_not_found -> failwith "Cannot build Route_not_found"

let to_uri route = snd (to_meth_and_path route) |> Uri.of_string

let index_uri = to_uri Index
let ws_show_uri game_id = to_uri (Ws_show game_id)
let game_create_uri game_id = to_uri Game_create
let game_show_uri game_id = to_uri (Game_show game_id)