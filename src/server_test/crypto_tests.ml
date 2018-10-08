open OUnit2
open Lib

open Test_lib.Test_util

let crypto_encode_decode_tests =
  let case ~secret ~salt ~msg =
    (Crypto.make ~salt secret, msg)
  and check (crypt, msg) =
    let enc = Crypto.encrypt_and_sign crypt msg in
    match Crypto.verify_and_decrypt crypt enc with
    | Ok(_, dec) -> ae ~printer:sp msg dec
    | Error(err) -> af err
  in
  let open CCList.Infix in
  let open Crypto in
  let cases = (0 --^ 128) >|= fun _ ->
    let rand_value () =
        match random_int 0 3 with
        | 0 -> "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        | 1 -> random_hex ()
        | 2 -> random_string 128
        | _ -> random_string 1024
    in
    case ~secret:(rand_value ()) ~salt:(rand_value ()) ~msg:(rand_value ())
  in
  cases_of check cases

let crypto_chain_test =
  (* mutually recursive encode/decode for testing next_iv from verify_and_decrypt *)
  let rec encode c iv decoded n i =
    let encoded = Crypto.encrypt_and_sign_with_iv c iv decoded in
    decode c encoded n i
  and decode c encoded n i =
    match Crypto.verify_and_decrypt c encoded with
    | Ok (niv, decoded) ->
      if i < n then
        encode c niv decoded n (i+1)
      else
        decoded
    | Error (err) -> err
  in
  let crypto = Crypto.make ~salt:"pink himalayan" "CNel6FficD3OkLO4JlTvxgNEDCq0aEIW36NMCSVvGycg93vQjhsoMwoz8pSJ9wjFg5E874PL0jzcTQkuMNMU3wjxTDXbaOOMundoEuojHgmnQ9JqnvI3LHs603gJDiEq" in
  let iv = Crypto.create_iv () in
  let msg = "A super secret message!" in
  let dec = encode crypto iv msg 100 0 in
  test_case (ae ~printer:sp msg dec)

let suite =
    Crypto.init ();
    [
        "encode/decode" >::: crypto_encode_decode_tests;
        "chain" >::: [crypto_chain_test];
    ]
