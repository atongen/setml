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
let ab msg got _test_ctxt = assert_bool msg got
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

module ConnKey = struct
    type t = (Clients.GameKey.t * Clients.PlayerKey.t)

    let make x y: t = (x, y)

    (*
     * Max game_id is 60_466_175
     * Least power of 2 greater than max game_id is 2^26 = 67_108_864
    *)
    let hash (x: t) =
        CCInt.hash (fst x) +
        CCInt.hash ((snd x) + 67_108_864)

    let equal (a: t) (b: t) =
        Clients.GameKey.equal (fst a) (fst b) &&
        Clients.PlayerKey.equal (snd a) (snd b)

    let to_string (x: t) =
        "game_id: " ^ string_of_int (fst x) ^ ", player_id: " ^ string_of_int (snd x)
end

module Counter = CCHashtbl.Make(ConnKey)

let make_conn counter game_id player_id =
    let key = ConnKey.make game_id player_id in
    fun w -> Counter.incr counter key

let random_int min max =
    let range = max - min in
    (Random.int range) + min

let random_game_id () = random_int 1_679_616 60_466_175

let game_id_array x =
  let open CCList.Infix in
  (0 -- x) >|= (fun idx -> 1_679_616 + idx)
  |> CCArray.of_list

let client_in_game_tests =
  let open CCList.Infix in
  let clients = Clients.make () in
  let counter = Counter.create 32 in
  13 -- 57 >>= (fun player_id ->
    let game_id = random_game_id () in
    let conn = make_conn counter game_id player_id in
    Clients.add clients game_id player_id conn;
    [
      test_case @@ ab "player in game" (Clients.in_game clients game_id player_id);
      test_case @@ ab "not player" (not @@ Clients.in_game clients game_id (player_id + 45));
      test_case @@ ab "not game" (not @@ Clients.in_game clients (random_game_id ()) player_id);
      test_case @@ ab "neither" (not @@ Clients.in_game clients (random_game_id ()) (player_id + 90));
    ]
  )

let client_send_tests =
  let open CCList.Infix in
  let clients = Clients.make () in
  let counter = Counter.create 16 in
  let game_ids = game_id_array 5 in
  let add_player_id player_id =
    let n = 4 in
    CCList.iter (fun i ->
      let idx = (player_id + i) mod n in
      let game_id = game_ids.(idx) in
      let conn = make_conn counter game_id player_id in
      Clients.add clients game_id player_id conn;
    ) (0 -- 1)
  in
  (* add players to games *)
  CCList.iter add_player_id (0 --^ 5);
  (* send messages *)
  CCList.iter (fun _ -> Clients.broadcast_send clients "broadcast") (0 --^ 3);
  CCList.iter (fun _ -> Clients.games_of_player_send clients 0 "games_of_player") (0 --^ 5);
  CCList.iter (fun _ -> Clients.game_send clients (game_ids.(3)) "game") (0 --^ 7);
  CCList.iter (fun _ -> Clients.player_send clients 0 "player") (0 --^ 11);

  (* setup tests *)
  let case ~game_id ~player_id ~count =
    let key = ConnKey.make game_id player_id in
    (key, count)
  and check (key, count) =
    let got_count = Counter.get_or counter key ~default:0 in
    ae ~printer:string_of_int count got_count in

  let cases = CCArray.to_list game_ids >>= (fun game_id ->
    0 --^ 6 >|= (fun player_id ->
      let count = match (game_id, player_id) with
      | (1_679_616, 0) -> 3+5+11
      | (1_679_616, 3) -> 3+5
      | (1_679_616, 4) -> 3+5
      | (1_679_617, 0) -> 3+5+11
      | (1_679_617, 1) -> 3+5
      | (1_679_617, 4) -> 3+5
      | (1_679_618, 1) -> 3
      | (1_679_618, 2) -> 3
      | (1_679_619, 2) -> 3+7
      | (1_679_619, 3) -> 3+7
      | (_, _) -> 0 in
      case ~game_id ~player_id ~count
    )
  ) in
  cases_of check cases

let base36_cases = [
    (         0,       "0");
    (         9,       "9");
    (        10,       "a");
    (        17,       "h");
    (        35,       "z");
    (        36,      "10");
    (        99,      "2r");
    (       100,      "2s");
    (       629,      "hh");
    (       999,      "rr");
    (      1000,      "rs");
    (      1295,      "zz");
    (      1296,     "100");
    (      9999,     "7pr");
    (     10000,     "7ps");
    (     22661,     "hhh");
    (     46655,     "zzz");
    (     46656,    "1000");
    (     99999,    "255r");
    (    100000,    "255s");
    (    815813,    "hhhh");
    (    999999,    "lflr");
    (   1000000,    "lfls");
    (   1679615,    "zzzz");
    (   1679616,   "10000");
    (   9999999,   "5yc1r");
    (  10000000,   "5yc1s");
    (  29369285,   "hhhhh");
    (  60466175,   "zzzzz");
    (  60466176,  "100000");
    (  99999999,  "1njchr");
    ( 100000000,  "1njchs");
    ( 999999999,  "gjdgxr");
    (1000000000,  "gjdgxs");
    (1057294277,  "hhhhhh");
    (2176782335,  "zzzzzz");
    (2176782336, "1000000");
  ]
let base36_encode_tests =
  let check (n, exp) =
    let got = Util.base36_of_int n in
    ae ~printer:sp exp got
  in
  cases_of check base36_cases

let base36_decode_tests =
  let check (exp, s) =
    let got = Util.int_of_base36 s in
    ae ~printer:string_of_int exp got
  in
  cases_of check base36_cases

let suite =
  "All" >::: [
    "crypto" >::: [
      "encode/decode" >::: crypto_encode_decode_tests;
      "chain" >::: [crypto_chain_test]
    ];
    "clients" >::: [
      "in_game" >::: client_in_game_tests;
      "send" >::: client_send_tests
    ];
    "base36" >::: [
      "encode" >::: base36_encode_tests;
      "decode" >::: base36_decode_tests
    ]
  ]

let _ =
  Crypto.init ();
  OUnit2.run_test_tt_main suite