module GameKey = CCInt
module PlayerKey = CCInt

module ConnKey = struct
    type t = (GameKey.t * PlayerKey.t)

    let make x y: t = (x, y)

    (*
     * Max game_id is 2_176_782_335
     * Least power of 2 greater than max game_id is 2^32 = 4_294_967_296
    *)
    let hash (x: t) =
        CCInt.hash (fst x) +
        CCInt.hash ((snd x) + 4_294_967_296)

    let equal (a: t) (b: t) =
        GameKey.equal (fst a) (fst b) &&
        PlayerKey.equal (snd a) (snd b)
end
module ConnTable = CCHashtbl.Make(ConnKey)

module PlayerSet = CCSet.Make(PlayerKey)
module PlayersOfGameTable = CCHashtbl.Make(GameKey)

type t = {
    conns: (Websocket_cohttp_lwt.Frame.t option -> unit) ConnTable.t;
    players_of_game: PlayerSet.t PlayersOfGameTable.t;
}

let make ?n:(m=32) () = {
    conns = ConnTable.create m;
    players_of_game = PlayersOfGameTable.create m;
}

let frame content = Some (Websocket_cohttp_lwt.Frame.create ~content ())

let send_t conn content = Lwt.wrap1 conn (frame content)
let send conn content = Lwt.async (fun () -> send_t conn content)

let send_key clients (key: ConnKey.t) content =
    match ConnTable.get clients.conns key with
    | Some (conn) -> send conn content
    | None -> ()

let game_send clients (game_id: GameKey.t) content =
    match PlayersOfGameTable.get clients.players_of_game game_id with
    | Some (player_ids) ->
        PlayerSet.to_seq player_ids (fun player_id ->
            let key = ConnKey.make game_id player_id in
            send_key clients key content
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
    PlayersOfGameTable.update clients.players_of_game ~f:(fun _ v ->
        match v with
        | Some (player_ids) -> Some(PlayerSet.add player_id player_ids)
        | None -> Some(PlayerSet.of_list [player_id])
    ) ~k:game_id

let remove clients game_id player_id =
    let key = ConnKey.make game_id player_id in
    ConnTable.remove clients.conns key;
    PlayersOfGameTable.update clients.players_of_game ~f:(fun _ v ->
        match v with
        | Some (player_ids) -> Some(PlayerSet.remove player_id player_ids)
        | None -> Some(PlayerSet.of_list [])
    ) ~k:game_id
