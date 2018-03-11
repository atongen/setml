type t = {
    session_id: string;
    player_id: int;
}

let make player_id = { session_id = Crypto.random_hex (); player_id }

let of_json_exn v =
    let open Yojson.Basic in
    let json = from_string v in
    let session_id = json |> Util.member "session_id" |> Util.to_string in
    let player_id = json |> Util.member "player_id" |> Util.to_int in
    { session_id; player_id }

let of_header crypto header cookie_key f =
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

let to_cookie_hdrs ?(discard=false) ?path ?domain ?secure ?http_only ~expiration cookie_key session crypto =
    let cookie = cookie_key, session.session_id in
    let hdr, val_ =
    Cohttp.Cookie.(Set_cookie_hdr.serialize
        (Set_cookie_hdr.make ~expiration ?path ?domain ?secure ?http_only cookie))
    in
    [(hdr, val_); ("vary", "cookie")]