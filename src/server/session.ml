type t = {
  session_id: string;
  player_id: int option;
  token: string;
  token_expiration: float;
}

let token_lifetime = 3600.

let make () =
  {
    session_id = Crypto.random_hex ();
    player_id = None;
    token = Crypto.random_hex ();
    token_expiration = Unix.time () +. token_lifetime
  }
let session_id_key = "s"
let player_id_key = "p"
let token_key = "t"
let token_expiration_key = "e"

let cookie_key = "__setml_session"

(* cookie expiration *)
let expiration = `Max_age (Int64.of_int (10 * 365 * 24 * 60 * 60))

exception Invalid_cookie of string

let token_expired session =
  session.token_expiration < Unix.time ()

let set_player_id session player_id =
  { session with player_id = Some (player_id) }

let refresh_token session =
  if token_expired session then
    { session with
      token = Crypto.random_hex ();
      token_expiration = Unix.time () +. token_lifetime
    }
  else session

let of_json x =
  let open Yojson.Basic in
  let json = from_string x in
  let session_id = json |> Util.member session_id_key |> Util.to_string in
  let player_id = json |> Util.member player_id_key |> Util.to_int_option in
  let token = json |> Util.member token_key |> Util.to_string in
  let token_expiration = json |> Util.member token_expiration_key |> Util.to_float in
  {
    session_id;
    player_id;
    token;
    token_expiration;
  }

let to_json session =
  let open Yojson.Basic in
  let player_id = match session.player_id with
    | Some (pid) -> `Int pid
    | None -> `Null
  in
  `Assoc [
    (session_id_key, `String session.session_id);
    (player_id_key, player_id);
    (token_key, `String session.token);
    (token_expiration_key, `Float session.token_expiration)
  ]
  |> to_string

let of_header crypto header =
  let cookies = Cohttp.Cookie.Cookie_hdr.extract header in
  try
    let value = CCList.Assoc.get ~eq:String.equal cookie_key cookies in
    match value with
    | Some (enc) -> (
        match Crypto.verify_and_decrypt crypto enc with
        | Ok(_, msg) -> Ok (of_json msg |> refresh_token)
        | Error(err) -> Error (Invalid_cookie ("Decryption error: " ^ err)))
    | None -> Ok (make ())
  with
  | Yojson.Basic.Util.Type_error (err, _) -> Error (Invalid_cookie ("JSON error: " ^ err))

let of_header_or_new crypto header =
  match of_header crypto header with
  | Ok (session) -> session
  | Error (err) ->
    ignore (print_endline (Printexc.to_string err));
    make ()

let to_header ?domain ?secure ?http_only session crypto =
  let json_str = to_json session in
  let value = Crypto.encrypt_and_sign crypto json_str in
  let cookie = cookie_key, value in
  Cohttp.Cookie.(Set_cookie_hdr.serialize
                   (Set_cookie_hdr.make ~expiration ~path:"/" ?domain ?secure ?http_only cookie))

let to_headers session crypto =
  let open Cohttp.Header in
  let cookie_key, header_val = to_header session crypto in
  add (init ()) cookie_key header_val

let set_player_id_headers session crypto player_id =
  let new_session = set_player_id session player_id in
  to_headers new_session crypto
