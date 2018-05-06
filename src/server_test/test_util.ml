open OUnit2
open Lwt
open Lwt.Infix

open Lib
open Shared

let (>>=?) m f =
  m >>= function
  | Ok x -> f x
  | Error (#Caqti_error.t as err) -> Lwt.fail (Caqti_error.Exn err)

let do_async_tests ?(name="lwt test") tests =
  let results =
    tests
    |> Lwt_list.map_s (fun (name, test) ->
        Logs.info (fun f -> f "Running %s" name);
        let res = Lwt.try_bind test
            (fun () -> return `Ok)
            (fun exn -> return (`Exn exn)) in
        res >|= (fun res -> (name, res))) in
  results >|= (fun results ->
      let ounit_tests =
        results
        |> List.map (fun (name, res) ->
            name >:: fun ctx ->
              match res with
              | `Ok -> ()
              | `Exn x -> raise x) in
      name >::: ounit_tests)

let assert_query_equal db exp q =
  Db.query_int db q >>=?  fun got ->
  assert_equal ~printer:string_of_int exp got;
  Lwt.return_unit

let refute_bool msg got = assert_bool msg (not got)

let card_not_equal c0 c1 = not (Card.equal c0 c1)

let refute_card_equal c0 c1 =
  assert_equal ~cmp:card_not_equal ~msg:"card not equal" ~printer:Card.to_string c0 c1

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

let string_of_int_list l =
  String.concat ", " (List.map string_of_int l)

let ae ~printer exp got _test_ctxt = assert_equal ~printer exp got
let ase exp got _test_ctxt = assert_equal ~printer:sp exp got
let aie exp got _test_ctxt = assert_equal ~printer:string_of_int exp got
let ab msg got _test_ctxt = assert_bool msg got
let af msg _test_ctxt = assert_failure msg
