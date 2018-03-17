open OUnit2
open Lib

let cases_of f =
  List.map @@ fun params -> test_case (f params)

let to_list s =
  let rec loop acc i =
    if i = -1 then acc
    else
      loop (s.[i] :: acc) (pred i)
  in loop [] (String.length s - 1)

let char_code_string c = string_of_int (Char.code c)

let char_list_to_string cl =
    let c = String.concat "', '" ((List.map char_code_string) cl) in
    "['" ^ c ^ "']"

let print_char_list v =
  to_list v |> char_list_to_string

let sp v =
  let l = String.length v in
  "'" ^ v ^ "' (" ^ string_of_int l ^ ")"

let ae ~printer exp got _test_ctxt = assert_equal ~printer exp got
let af msg _test_ctxt = assert_failure msg

let crypto_encode_decode_tests =
  let case ~secret ~salt ~msg =
    (Crypto.make ~salt secret, msg)
  and check (crypt, msg) =
    let enc = Crypto.encrypt_and_sign crypt msg in
    match Crypto.verify_and_decrypt crypt enc with
    | Ok(_, dec) -> ae ~printer:sp msg dec
    | Error(err) -> af err
  in
  cases_of check [
    case ~secret: "a"
         ~salt:   "a"
         ~msg:    "a"
  ; case ~secret: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
         ~salt:   "a"
         ~msg:    "a"
  ; case ~secret: "7TsXNpzgVE7MUqcZVK7LoojHx1JHKiN3pFd03hUkyPLYiF5jXX077Q9Pe1T9QTqsECrOgWDWOnJn4kcsFdwluzcPUlELGMYetUW6Q32II8hkXXbBVSWX0k04VYwIiYef"
         ~salt:   "a"
         ~msg:    "a"
  ; case ~secret: "e|ym)vztgRhLnyPypL,GDR5Av5mNqxq+ujPz)A]9?I22sS # ?W^!)?QY,,.V1cnh|v0T0k=3Xeul?,MUc&GQe[>\!AfTC0(pGo0 P.`&|YYIT9Xlxj^\"oa'6-iZ%nww"
         ~salt:   "=,{%)Uc=Rkfa_8aN99e4Cd'44/{;077:d /(DF@gY|ZVNg-o]1E}30;il0lUulUL8%4# J1yJtaxhtU$-mpF}a^d^n=.>+8<QJ)U%s*,2a'r^+&<4/%W]tyZ~opv^\l-"
         ~msg:    "a"
  ; case ~secret: "e|ym)vztgRhLnyPypL,GDR5Av5mNqxq+ujPz)A]9?I22sS # ?W^!)?QY,,.V1cnh|v0T0k=3Xeul?,MUc&GQe[>\!AfTC0(pGo0 P.`&|YYIT9Xlxj^\"oa'6-iZ%nww"
         ~salt:   "=,{%)Uc=Rkfa_8aN99e4Cd'44/{;077:d /(DF@gY|ZVNg-o]1E}30;il0lUulUL8%4# J1yJtaxhtU$-mpF}a^d^n=.>+8<QJ)U%s*,2a'r^+&<4/%W]tyZ~opv^\l-"
         ~msg:    "-t2j (4wXiSzSX>g^/h)O[jp)W\lOe\X{sg08r#\hB, @X$jVHP:;RwM;bKm+[p%^e19%jhNhN6h'6v8+) FhjRgrOr| 299v,4M$;Us#'l@87|k44mKz_Ja+@JOPT=A"
  ; case ~secret: "X$\"nU\"<uAK'BE'=uCp&f d?eHAg`+=zQg;gi]VR#nqd&\DhYoxM0^)G>@nYj^s*<<sH%fr?zj,HvnZkopQ2hZPX>;/l:f\*'r2RBU!urU?|TC,N0(Q&}!ZC2((.:{1BMiwvJc"
         ~salt:   "l-]t~E3y8d;;hkqMl2ruwIi>!'-2\_O`mYkl,58ckwBiIMI/t/3m[[{Xw+CbP7V+R;^/Qa,(leDpuvv4qtmYh<;Zdn3lW#c_\sUI,m0A!gt~Nuxm.]}N f8B}?E2CAMWq'c>2"
         ~msg:    "'R2bR }~\p, s4]<|]TS2@0wR%\7bjz84xWIeiF`&ll$dftM4c$_KEB3rLU/A7s!\Hm!I?'um

                   B>s$&<;bkYFS&S(GE)Dp7D(|{)g?`s2D^!V5wcGPM\"8]S3U|1>&l70Og6]11[808\"]W0{W[Kc

                   uhN2ae~WQ9Z3FtS_Y`hP?M=S(<5dmKA7W3H5ve2sNs\"QuGfPXxE6V$=lF4/GUD)!yE}4Aj88N

                   w*d9/B)gDp|S lSqg`__lURp-R;K6QO~!0cw%A-d6^zh6j%BJ0~h8TmnzwF{+hs!*QGOQze#c

                   9A\d^;&RH9\"]: AXUI$J\;WuO6oZ |QzgB'V>PV}W3~k-Rx>D-^y=m<OVsqfj,LR&xQnxGYmw

                   M6n#Wm?w03soly!IzSK!uslLxcF{PojF=\"XWFMJ]_)SPZiNc_/w ;A1J#OL`@n-#vZjmR$\7Z

                   FR*wLP$92nzBN]e]y+k%0Q;!20Jc>A=0rqy:L\mp/9KY4X:O2>!3.nm0!X~I-]Dr=~\4uLzDh
                "
  ]

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
  "All" >::: [
    "crypto" >::: [
      "encode/decode" >::: crypto_encode_decode_tests;
      "chain" >::: [crypto_chain_test]
    ]
  ]

let _ =
  Crypto.init ();
  OUnit2.run_test_tt_main suite