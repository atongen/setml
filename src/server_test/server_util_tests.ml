open OUnit2
open Lib
open Shared

open Test_util

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
    let got = Server_util.base36_of_int n in
    ae ~printer:sp exp got
  in
  cases_of check base36_cases

let base36_decode_tests =
  let check (exp, s) =
    let got = Server_util.int_of_base36 s in
    ae ~printer:string_of_int exp got
  in
  cases_of check base36_cases

let encode_decode_tests =
    let open CCList.Infix in
    let cases = 2 -- 62 >>= fun b ->
        [1; 100; 1_000; 10_000; 100_000; 1_000_000] >>= fun s ->
            let v = 1_000 * s in
            [-1; 1] >|= fun i ->
                (i * (Random.int v) + i * v, b)
    in
    let check (n, b) =
        let encoded = Server_util.base_of_int n b in
        let decoded = Server_util.int_of_base encoded b in
        ae ~printer:string_of_int n decoded
    in
    cases_of check cases

let suite = [
    "base36_encode" >::: base36_encode_tests;
    "base36_decode" >::: base36_decode_tests;
    "encode_decode" >::: encode_decode_tests;
]
