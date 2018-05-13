open OUnit2
open Lib
open Shared

open Test_lib.Test_util

module ConnKey = struct
  type t = (Clients.GameKey.t * Clients.PlayerKey.t)

  let make x y: t = (x, y)

  let hash (x: t) =
    CCInt.hash (fst x) +
    CCInt.hash ((snd x) + 4_294_967_296)

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

let random_id () = Crypto.random_int 1 999_999

let client_in_game_tests =
  let open CCList.Infix in
  let clients = Clients.make () in
  let counter = Counter.create 32 in
  13 -- 57 >>= (fun player_id ->
      let game_id = random_id () in
      let conn = make_conn counter game_id player_id in
      Clients.add clients game_id player_id conn;
      [
        test_case @@ ab "player in game" (Clients.in_game clients game_id player_id);
        test_case @@ ab "not player" (not @@ Clients.in_game clients game_id (player_id + 45));
        test_case @@ ab "not game" (not @@ Clients.in_game clients (random_id ()) player_id);
        test_case @@ ab "neither" (not @@ Clients.in_game clients (random_id ()) (player_id + 90));
      ]
    )

let client_send_tests =
  let open CCList.Infix in
  let clients = Clients.make () in
  let counter = Counter.create 16 in
  let game_ids = [|1000; 1001; 1002|] in
  let player_ids = 1 -- 100 in
  let add game_id player_id =
    let conn = make_conn counter game_id player_id in
    Clients.add clients game_id player_id conn
  in
  CCList.iter (fun i ->
    if i mod 15 = 0 then (
        add game_ids.(0) i;
        add game_ids.(1) i;
        add game_ids.(2) i
    ) else if i mod 5 = 0 then (
        add game_ids.(1) i
    ) else if i mod 3 = 0 then (
        add game_ids.(2) i
    )
  ) player_ids;

  (* send messages *)
  CCList.iter (fun _ -> Clients.game_send clients (game_ids.(0)) "15") (0 --^ 4);
  CCList.iter (fun _ -> Clients.game_send clients (game_ids.(1)) "5") (0 --^ 2);
  CCList.iter (fun _ -> Clients.game_send clients (game_ids.(2)) "3") (0 --^ 1);

  (* setup tests *)
  let case ~game_id ~player_id ~count =
    let key = ConnKey.make game_id player_id in
    (key, count)
  and check (key, count) =
    let got_count = Counter.get_or counter key ~default:0 in
    ae ~printer:string_of_int count got_count in

  let cases = CCArray.to_list game_ids >>= (fun game_id ->
      player_ids >|= (fun player_id ->
          let count = if game_id = game_ids.(0) && player_id mod 15 = 0 then
            4
          else if game_id = game_ids.(1) && player_id mod 5 = 0 then
            2
          else if game_id = game_ids.(2) && player_id mod 3 = 0 then
            1
          else
            0
          in
          case ~game_id ~player_id ~count
        )
    ) in
  cases_of check cases

let suite = [
    "in_game" >::: client_in_game_tests;
    "send" >::: client_send_tests
]
