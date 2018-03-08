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

let hmac secret salt value =
  value
  |> Cstruct.of_string
  |> Nocrypto.Hash.mac `SHA256 ~key:(derive_key secret salt)
  |> Nocrypto.Base64.encode

let make ?salt:(s="session-salt") secret cookie_key =
  {
    secret;
    cookie_key;
    salt = s;
    encryption_key = Cstruct.to_string (hmac secret s "encryption key ") ^ secret |> Cstruct.of_string |> Nocrypto.Cipher_block.AES.CBC.of_secret;
    authentication_key = Cstruct.of_string (Cstruct.to_string (hmac secret s "authentication key ") ^ secret)
  }

let encrypt session iv msg =
  let open Nocrypto.Cipher_block.AES.CBC in
  let key = session.encryption_key in
  let encrypted = encrypt ~key ~iv msg in
  Cstruct.concat [iv; encrypted]

let decrypt session msg =
  let open Nocrypto.Cipher_block.AES.CBC in
  let (iv, data) = Cstruct.split msg 16 in (* aes cbs block length is 16 *)
  let decrypted = decrypt ~key:session.encryption_key ~iv data in
  let niv = next_iv ~iv msg in
  (niv, Cstruct.to_string decrypted)

let signature session msg =
  msg |> Nocrypto.Hash.mac `SHA256 ~key:session.authentication_key

let sign session msg =
  Cstruct.concat [signature session msg; msg]

let verify session msg =
  let value = Cstruct.of_string msg in
  let (s, data) = Cstruct.split value 44 in (* sha256 is 44 bytes *)
  (s = signature session data, data)

let random_hex_string n =
  let hexa = "0123456789abcdef" in
  let s = Bytes.create (n * 8) in
  for i = 0 to 15 do
    Bytes.set s i (String.get hexa (Random.int 16))
  done;
  Bytes.to_string s

let verify_and_decrypt session msg =
  match verify session msg with
  | (true, data) -> Ok(decrypt session data)
  | (false, _) -> Error("message not verified")


let decode_request_headers (session: t) (headers: Cohttp.Header.t) =
    let cookies = Cohttp.Cookie.Cookie_hdr.extract headers in
    try
      let signed_and_encrypted = List.assoc session.cookie_key cookies in
      match verify_and_decrypt session signed_and_encrypted with
      | Ok(niv, msg) -> ""
      | Error(msg) -> msg
    with Not_found -> ""
        (* CCHashtbl.of_list [("session_id", gen_session_id ())] *)