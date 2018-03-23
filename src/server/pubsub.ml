type t = {
    conn: Postgresql.connection;
    clients: Clients.t
}

let make conninfo clients = {
    conn = new Postgresql.connection ~conninfo ();
    clients
}

let subscribe pubsub game_id =
    let game_id_int = Util.int_of_base36 game_id in
    pubsub.conn#send_query @@ "listen game_" ^ string_of_int game_id_int ^ ";"

let unsubscribe pubsub game_id =
    let game_id_int = Util.int_of_base36 game_id in
    pubsub.conn#send_query @@ "unlisten game_" ^ string_of_int game_id_int ^ ";"

let handle_present pubsub game_id player_id present =
    if present then
        Clients.game_send pubsub.clients game_id ("Player " ^ string_of_int player_id ^ " joined the game.")
    else
        Clients.game_send pubsub.clients game_id ("Player " ^ string_of_int player_id ^ " left the game.")

let handle_notification pubsub payload =
    match Yojson.Safe.from_string payload with
    | `Assoc ["type", `String "present"; "game_id", `Int game_id_int; "player_id", `Int player_id; "value", `Bool present] ->
        let game_id = Util.base36_of_int game_id_int in
        handle_present pubsub game_id player_id present
    | _ -> ()

let start pubsub =
    let rec aux pubsub =
        let socket : Unix.file_descr = Obj.magic pubsub.conn#socket in
        let _ = Unix.select [socket] [] [] 1. in
        pubsub.conn#consume_input;
        match pubsub.conn#notifies with
        | Some { extra } ->
            ignore (print_endline "handling notification");
            handle_notification pubsub extra;
            aux pubsub
        | None ->
            ignore (print_endline "pubsub empty");
            aux pubsub
    in
    aux pubsub