module CHT = CCHashtbl.Make(CCInt)
module CS = CCSet.Make(CCInt)

type t = {
    conns: (Websocket_cohttp_lwt.Frame.t option -> unit) CHT.t;
    players_of_game: CS.t CHT.t;
    games_of_player: CS.t CHT.t;
}

let make () =
    {
        conns = CHT.create 32;
        players_of_game = CHT.create 32;
        games_of_player = CHT.create 32;
    }

let broadcast clients content =
    CHT.values clients.conns (fun send ->
        send @@ Some (Websocket_cohttp_lwt.Frame.create ~content ())
    )

let add clients player_id game_id send =
    CHT.add clients.conns player_id send;
    CHT.update clients.players_of_game ~f:(fun k v ->
        match v with
        | Some (set) -> Some(CS.add player_id set)
        | None -> Some(CS.of_list [player_id])
    ) ~k:game_id;
    CHT.update clients.games_of_player ~f:(fun k v ->
        match v with
        | Some (set) -> Some(CS.add game_id set)
        | None -> Some(CS.of_list [game_id])
    ) ~k:player_id

