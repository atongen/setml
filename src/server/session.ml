type t = {
    session_id: string;
    player_id: int;
}

let make player_id = { session_id = Crypto.random_hex (); player_id }
let session_id_key = "session_id"
let player_id_key = "player_id"
let default_cookie_key = "__setml_session"

let of_json_exn x =
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

let of_headers crypto header cookie_key f =
    let cookies = Cohttp.Cookie.Cookie_hdr.extract header in
    try
        let value = List.assoc cookie_key cookies in
        match Crypto.verify_and_decrypt crypto value with
        | Ok(_, msg) -> begin
            try of_json_exn msg
            with Yojson.Basic.Util.Type_error(_, _) -> make (f ())
        end
        | Error(err) -> make (f ())
    with Not_found -> make (f ())

let to_headers ?expiration ?path ?domain ?secure ?http_only ?cookie_key session crypto =
    let cookie_key =
        match cookie_key with
        | None -> default_cookie_key
        | Some ck  -> ck
    in
    let json_str = to_json session in
    let value = Crypto.encrypt_and_sign crypto json_str in
    let cookie = cookie_key, value in
    let hdr, val_ =
    Cohttp.Cookie.(Set_cookie_hdr.serialize
        (Set_cookie_hdr.make ?expiration ?path ?domain ?secure ?http_only cookie))
    in
    [(hdr, val_); ("vary", "cookie")]