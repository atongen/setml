module GameKey = CCString
module PlayerKey = CCInt

module ConnKey = struct
    type t = (GameKey.t * PlayerKey.t)

    let make x y: t = (x, y)

    let hash (x: t) =
        GameKey.hash (fst x) + PlayerKey.hash (snd x)
    let equal (a: t) (b: t) =
        GameKey.equal (fst a) (fst b) &&
        PlayerKey.equal (snd a) (snd b)

    let to_string (x: t) =
        "game_id: " ^ (fst x) ^ ", player_id: " ^ string_of_int (snd x)
end
module ConnTable = CCHashtbl.Make(ConnKey)

module GameSet = CCSet.Make(GameKey)
module GamesOfPlayerTable = CCHashtbl.Make(PlayerKey)
module PlayerSet = CCSet.Make(PlayerKey)
module PlayersOfGameTable = CCHashtbl.Make(GameKey)

type t = {
    conns: (Websocket_cohttp_lwt.Frame.t option -> unit) ConnTable.t;
    games_of_player: GameSet.t GamesOfPlayerTable.t;
    players_of_game: PlayerSet.t PlayersOfGameTable.t;
}

let make ?n:(m=32) () = {
    conns = ConnTable.create m;
    games_of_player = GamesOfPlayerTable.create m;
    players_of_game = PlayersOfGameTable.create m;
}

let frame content = Some (Websocket_cohttp_lwt.Frame.create ~content ())

let send conn content = conn (frame content)

(*
let send_t conn content = Lwt.wrap1 conn (frame content)
let send conn content = Lwt.async (fun () -> send_t conn content)

let send_join clients keys content =
    let open CCList.Infix in
    (keys >|= fun key ->
        match ConnTable.get clients.conns key with
        | Some (conn) -> send_t conn content
        | None -> Lwt.return_unit)
    |> Lwt.join

let send_join_async clients keys content =
    Lwt.async (fun () -> send_join clients keys content)
*)

let send_key clients (key: ConnKey.t) content =
    match ConnTable.get clients.conns key with
    | Some (conn) -> send conn content
    | None -> ()

let broadcast_send clients content =
    ConnTable.values clients.conns (fun f -> send f content)

let game_send clients (game_id: GameKey.t) content =
    match PlayersOfGameTable.get clients.players_of_game game_id with
    | Some (player_ids) ->
        PlayerSet.to_seq player_ids (fun player_id ->
            let key = ConnKey.make game_id player_id in
            send_key clients key content
        )
    | None -> ()

let player_send clients (player_id: PlayerKey.t) content =
    match GamesOfPlayerTable.get clients.games_of_player player_id with
    | Some (game_ids) ->
        GameSet.to_seq game_ids (fun game_id ->
            let key = ConnKey.make game_id player_id in
            send_key clients key content
        )
    | None -> ()

let games_of_player_send clients (player_id: PlayerKey.t) content =
    match GamesOfPlayerTable.get clients.games_of_player player_id with
    | Some (game_ids) ->
        GameSet.to_seq game_ids (fun game_id ->
            game_send clients game_id content
        )
    | None -> ()

let in_game clients (game_id: GameKey.t) (player_id: PlayerKey.t) =
    let key = ConnKey.make game_id player_id in
    ConnTable.mem clients.conns key

let game_has_players clients (game_id: GameKey.t) =
    match PlayersOfGameTable.get clients.players_of_game game_id with
    | Some (player_set) -> PlayerSet.cardinal player_set > 0
    | None -> false

let add clients game_id player_id send =
    let key = ConnKey.make game_id player_id in
    ConnTable.add clients.conns key send;
    PlayersOfGameTable.update clients.players_of_game ~f:(fun k v ->
        match v with
        | Some (player_ids) -> Some(PlayerSet.add player_id player_ids)
        | None -> Some(PlayerSet.of_list [player_id])
    ) ~k:game_id;
    GamesOfPlayerTable.update clients.games_of_player ~f:(fun k v ->
        match v with
        | Some (game_ids) -> Some(GameSet.add game_id game_ids)
        | None -> Some(GameSet.of_list [game_id])
    ) ~k:player_id

let remove clients game_id player_id =
    let key = ConnKey.make game_id player_id in
    ConnTable.remove clients.conns key;
    PlayersOfGameTable.update clients.players_of_game ~f:(fun k v ->
        match v with
        | Some (player_ids) -> Some(PlayerSet.remove player_id player_ids)
        | None -> Some(PlayerSet.of_list [])
    ) ~k:game_id;
    GamesOfPlayerTable.update clients.games_of_player ~f:(fun k v ->
        match v with
        | Some (player_ids) -> Some(GameSet.remove game_id player_ids)
        | None -> Some(GameSet.of_list [])
    ) ~k:player_id

let add_or_replace clients game_id player_id send =
    if in_game clients game_id player_id then remove clients game_id player_id;
    add clients game_id player_id send