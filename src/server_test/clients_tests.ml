open OUnit2
open Lib
open Shared

open Test_lib.Test_util

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

let suite = [
    "in_game" >::: client_in_game_tests;
    "send" >::: client_send_tests
]
