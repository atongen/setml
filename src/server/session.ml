type t = {
    secret: string;
    cookie_key: string;
    salt: string;
    encryption_key: string;
    authentication_key: string
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
  |> Cstruct.to_string

let make ?salt:(s="session-salt") secret cookie_key =
  {
    secret;
    cookie_key;
    salt = s;
    encryption_key = hmac secret s "encryption key " ^ secret;
    authentication_key = hmac secret s "authentication key " ^ secret
  }

let encrypt session msg =
let Nocrypto.Cipher_block.AES.CBC.of_secret (Cstruct.of_string session.encryption_key)


let random_hex_string n =
  let hexa = "0123456789abcdef" in
  let s = Bytes.create (n * 8) in
  for i = 0 to 15 do
    Bytes.set s i (String.get hexa (Random.int 16))
  done;
  Bytes.to_string s
let gen_session_id () = random_hex_string 16

(*
let decode_request_headers (session: t) (headers: Cohttp.Header.t) =
    let cookies = Cohttp.Cookie.Cookie_hdr.extract headers in
    try
      let encoded = List.assoc session.cookie_key cookies in

    with Not_found ->
        CCHashtbl.of_list [("session_id", gen_session_id ())]


*)