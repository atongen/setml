type t = {
    conn: Postgresql.connection;
    clients: Clients.t
}

let make conninfo clients = {
    conn = new Postgresql.connection ~conninfo ();
    clients
}

let subscribe pubsub game_id =
    try
        let game_id_int = Util.int_of_base36 game_id in
        pubsub.conn#send_query @@ "listen game_" ^ string_of_int game_id_int ^ ";"
    with
    | Postgresql.Error (e) -> prerr_endline (Postgresql.string_of_error e)
    | e -> prerr_endline (Printexc.to_string e)

let unsubscribe pubsub game_id =
    try
        let game_id_int = Util.int_of_base36 game_id in
        pubsub.conn#send_query @@ "unlisten game_" ^ string_of_int game_id_int ^ ";"
    with
    | Postgresql.Error (e) -> prerr_endline (Postgresql.string_of_error e)
    | e -> prerr_endline (Printexc.to_string e)

let handle_present pubsub game_id player_id present =
    let msg = if present then "joined" else "left" in
    Clients.game_send pubsub.clients game_id ("Player " ^ string_of_int player_id ^ " " ^ msg ^ " the game.")

let handle_notification pubsub payload =
    ignore (print_endline @@ "handle_notification: " ^ payload);
    match Yojson.Safe.from_string payload with
    | `Assoc ["type", `String "present"; "game_id", `Int game_id_int; "player_id", `Int player_id; "value", `Bool present] ->
        let game_id = Util.base36_of_int game_id_int in
        handle_present pubsub game_id player_id present
    | _ -> ()

let start pubsub =
    try
        while true do
            let socket : Unix.file_descr = Obj.magic pubsub.conn#socket in
            let _ = Unix.select [socket] [] [] 1. in
            pubsub.conn#consume_input;
            ignore (print_endline @@ "waiting!");
            let rec aux () =
                match pubsub.conn#notifies with
                | Some { extra } ->
                    ignore (print_endline @@ "consume_notification: " ^ extra);
                    handle_notification pubsub extra;
                    aux ()
                | None -> ()
            in
            aux ()
        done
    with
    | Postgresql.Error (e) -> prerr_endline (Postgresql.string_of_error e)
    | e -> prerr_endline (Printexc.to_string e)

(*
let read_notifications pubsub =
    let open Lwt.Infix in
    let read_notification pubsub =
        ignore (print_endline "Reading!");
        pubsub.conn#consume_input;
        match pubsub.conn#notifies with
        | Some { extra } -> Lwt.return (Some (extra))
        | None -> Lwt.return None
    in
    let rec aux () = read_notification pubsub
        >>= Lwt.wrap2 handle_notification pubsub
        >>= aux
    in aux ()
*)