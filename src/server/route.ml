type t =
    | Ws_show of string
    | Game_create
    | Game_show of string
    | Static
    | Not_found

let path_parts path =
    Str.split (Str.regexp "\/+") path
    |> Array.of_list

let of_req req =
    let meth = Cohttp.Request.meth req in
    let uri = Cohttp.Request.uri req in
    let path = Uri.path uri in
    let parts = path_parts path in
    let n = Array.length parts in
    if n == 1 && String.equal parts.(0) "games" && meth == `POST then
        Game_create
    else if n == 2 && String.equal parts.(0) "games" && meth == `GET then
        Game_show (parts.(1))
    else if n == 3 && String.equal parts.(0) "games" && String.equal parts.(2) "ws" && meth == `GET then
        Ws_show (parts.(1))
    else if meth == `GET then
        Static
    else
        Not_found