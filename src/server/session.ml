type t = {
    session_id: string;
    player_id: int;
}

let make player_id = { session_id = Crypto.random_hex (); player_id }
let session_id_key = "session_id"
let player_id_key = "player_id"
let default_cookie_key = "__setml_session"

exception Invalid_cookie of string

let of_json x =
    let open Yojson.Basic in
    let json = from_string x in
    let session_id = json |> Util.member session_id_key |> Util.to_string in
    let player_id = json |> Util.member player_id_key |> Util.to_int in
    { session_id; player_id }

let to_json session =
    let open Yojson.Basic in
    `Assoc [
        (session_id_key, `String session.session_id);
        (player_id_key, `Int session.player_id)
    ]
    |> to_string

let of_header ?cookie_key crypto header =
    let cookie_key =
        match cookie_key with
        | None -> default_cookie_key
        | Some ck -> ck
    in
    let cookies = Cohttp.Cookie.Cookie_hdr.extract header in
    try
        let value = List.assoc cookie_key cookies in
        match Crypto.verify_and_decrypt crypto value with
        | Ok(_, msg) -> Ok (of_json msg)
        | Error(err) -> Error (Invalid_cookie ("Decryption error: " ^ err))
    with
    | Not_found -> Error (Not_found)
    | Yojson.Basic.Util.Type_error (err, _) -> Error (Invalid_cookie ("JSON error: " ^ err))

let to_header ?expiration ?path ?domain ?secure ?http_only ?cookie_key session crypto =
    let cookie_key =
        match cookie_key with
        | None -> default_cookie_key
        | Some ck -> ck
    in
    let json_str = to_json session in
    let value = Crypto.encrypt_and_sign crypto json_str in
    let cookie = cookie_key, value in
    Cohttp.Cookie.(Set_cookie_hdr.serialize
        (Set_cookie_hdr.make ?expiration ?path ?domain ?secure ?http_only cookie))