type action =
    | Subscribe of int
    | Unsubscribe of int

type t = {
    conn: Postgresql.connection;
    clients: Clients.t;
    actions: action CCBlockingQueue.t;
}

let make ?(n=32) conninfo clients = {
    conn = new Postgresql.connection ~conninfo ();
    clients;
    actions = CCBlockingQueue.create n
}

let handle_present pubsub game_id player_id present =
    let msg = if present then "joined" else "left" in
    Lwt_preemptive.run_in_main (fun () ->
        Lwt.return (
            Clients.game_send pubsub.clients game_id ("Player " ^ string_of_int player_id ^ " " ^ msg ^ " the game.")
        )
    )

let handle_notification pubsub payload =
    ignore (print_endline @@ "handle_notification: " ^ payload);
    match Yojson.Safe.from_string payload with
    | `Assoc ["type", `String "present"; "game_id", `Int game_id; "player_id", `Int player_id; "value", `Bool present] ->
        handle_present pubsub game_id player_id present
    | _ -> print_endline @@ "Unknown notification type: " ^ payload

let subscribe pubsub game_id =
    Subscribe game_id |>
    CCBlockingQueue.push pubsub.actions

let unsubscribe pubsub game_id =
    Unsubscribe game_id |>
    CCBlockingQueue.push pubsub.actions

let handle_result (conn: Postgresql.connection) (result: Postgresql.result) =
    let open Postgresql in
    let open Printf in
    match result#status with
    | Empty_query -> printf "Empty query\n"
    | Command_ok -> printf "Command ok [%s]\n" result#cmd_status
    | Tuples_ok ->
        printf "Tuples ok\n";
        printf "%i tuples with %i fields\n" result#ntuples result#nfields;
        print_endline (String.concat ";" result#get_fnames_lst);
        for tuple = 0 to result#ntuples - 1 do
            for field = 0 to result#nfields - 1  do
            printf "%s, " (result#getvalue tuple field)
            done;
            print_newline ()
        done
    | Copy_out -> printf "Copy out, not handled!\n"
    | Copy_in -> printf "Copy in, not handled!\n"
    | Bad_response -> printf "Bad response: %s\n" result#error; conn#reset
    | Nonfatal_error -> printf "Non fatal error: %s\n" result#error
    | Fatal_error -> printf "Fatal error: %s\n" result#error
    | Copy_both -> printf "Copy in/out, not handled!\n"
    | Single_tuple -> printf "Single tuple, not handled!\n"

let rec flush_result pubsub =
    match pubsub.conn#get_result with
    | Some result ->
        handle_result pubsub.conn result;
        flush_result pubsub
    | None -> ()

let rec hold pubsub f =
    let fd = Obj.magic pubsub.conn#socket in
    pubsub.conn#consume_input;
    if pubsub.conn#is_busy then
        let _ = Unix.select [fd] [] [] 0.25 in
        hold pubsub f
    else
        f ()

let get_notifications pubsub =
    hold pubsub (fun () ->
        let rec aux pubsub i =
            match pubsub.conn#notifies with
            | Some { Postgresql.Notification.name; pid; extra } ->
                Printf.printf "Notication from backend %i: [%s] [%s]\n" pid name extra;
                handle_notification pubsub extra;
                aux pubsub (i + 1)
            | None -> i
        in aux pubsub 0
    )

let empty_query pubsub query =
    ignore (print_endline ("query: " ^ query));
    pubsub.conn#send_query @@ query;
    hold pubsub (fun () -> flush_result pubsub)

let handle_action pubsub action =
    let query = match action with
    | Subscribe (game_id) -> "listen game_" ^ string_of_int game_id ^ ";"
    | Unsubscribe (game_id) -> "unlisten game_" ^ string_of_int game_id ^ ";"
    in empty_query pubsub query

let start pubsub =
    let rec aux () =
        match CCBlockingQueue.try_take pubsub.actions with
        | Some (action) ->
            handle_action pubsub action;
            aux ()
        | None ->
            let i = get_notifications pubsub in
            if i = 0 then Unix.sleepf 0.25;
            aux ()
    in
    aux ()