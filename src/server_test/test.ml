open OUnit2
open Lwt
open Lwt.Infix

open Test_util
open Lib
open Shared

let _ =
  Crypto.init ();
  OUnit2.run_test_tt_main Test_sync.suite;
  Lwt_main.run (begin
      Caqti_lwt.connect (Uri.of_string "postgresql://atongen:at1234@localhost:5435/setml_test") >>=? fun db ->
      Db.delete_all db >>=? fun () ->
      Test_async.ats db >|= OUnit2.run_test_tt_main
    end)
