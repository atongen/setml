type t = {
    secret: string;
    cookie_key: string;
    salt: string;
    encryption_key: Nocrypto.Cipher_block.AES.CBC.key;
    authentication_key: Cstruct.t
}

let derive_key secret salt =
  Nocrypto.Hash.mac `SHA256
    ~key:(Cstruct.of_string secret)
    (Cstruct.of_string salt)

let print_cstruct s name =
  let h = Nocrypto.Base64.encode s |> Cstruct.to_string in
  let l = string_of_int (Cstruct.len s) in
  ignore (print_endline (name ^ ": " ^ h ^ " (" ^ l ^ ")"))

let hmac secret salt value =
  value
  |> Cstruct.of_string
  |> Nocrypto.Hash.mac `SHA256 ~key:(derive_key secret salt)

let make ?salt:(s="session-salt") secret cookie_key =
  let encryption_key =
    let digest = hmac secret s "encryption key" in
    Cstruct.sub digest 0 16
    |> Nocrypto.Cipher_block.AES.CBC.of_secret in
  let authentication_key = hmac secret s "authentication key" in
  {
    secret;
    cookie_key;
    salt = s;
    encryption_key;
    authentication_key;
  }

let padded_data msg block_size =
  let len = Cstruct.len msg in
  let new_len = ((len / block_size) + 1) * block_size in
  let t = Cstruct.create new_len in
  (Cstruct.blit msg 0 t 0 len; t)

let encrypt session iv msg =
  let open Nocrypto.Cipher_block.AES.CBC in
  let key = session.encryption_key in
  let encrypted = encrypt ~key ~iv (padded_data msg 16) in
  Cstruct.concat [iv; encrypted]

let decrypt session msg =
  let open Nocrypto.Cipher_block.AES.CBC in
  let (iv, data) = Cstruct.split msg 16 in (* aes cbs block length is 16 bytes *)
  let decrypted = decrypt ~key:session.encryption_key ~iv data in
  let (_, msg) = Cstruct.split decrypted 16 in (* discard 16 byte block padding *)
  let niv = next_iv ~iv data in
  (niv, Cstruct.to_string msg)

let signature session msg =
  msg |> Nocrypto.Hash.mac `SHA256 ~key:session.authentication_key

let sign session msg =
  let s = signature session msg in
  Cstruct.concat [s; msg]

let verify session msg =
  let (s, data) = Cstruct.split msg 32 in (* sha256 is 32 bytes *)
  (Cstruct.equal s (signature session data), data)

let random_string n =
  let s = Bytes.create n in
  for i = 0 to Bytes.length s - 1 do
    Bytes.set s i (Char.chr (Random.int 256))
  done;
  Bytes.to_string s

let create_iv () =
  let secret = random_string 16 in
  let salt = "session-salt" in
  let value = string_of_float (Unix.gettimeofday ()) in
  let digest = hmac secret salt value in
  Cstruct.sub digest 0 16

let verify_and_decrypt session msg =
  let decoded = Cstruct.of_string msg |> Nocrypto.Base64.decode in
  match decoded with
  | Some(value) -> begin
      match verify session value with
      | (true, data) -> Ok(decrypt session data)
      | (false, _) -> Error("message not verified")
    end
  | None -> Error("invalid encoding")

let encrypt_and_sign_with_iv session iv msg =
  let data = encrypt session iv (Cstruct.of_string msg) in
  let encrypted = Cstruct.concat [iv; data] in
  sign session encrypted
  |> Nocrypto.Base64.encode
  |> Cstruct.to_string

let encrypt_and_sign session msg =
  encrypt_and_sign_with_iv session (create_iv ()) msg

(*
let decode_request_headers (session: t) (headers: Cohttp.Header.t) =
    let cookies = Cohttp.Cookie.Cookie_hdr.extract headers in
    try
      let signed_and_encrypted = List.assoc session.cookie_key cookies in
      match verify_and_decrypt session signed_and_encrypted with
      | Ok(niv, msg) -> ""
      | Error(msg) -> msg
    with Not_found -> ""
        (* CCHashtbl.of_list [("session_id", gen_session_id ())] *)
*)