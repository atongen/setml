open Shared
open Shared.Messages
open Server_messages

type action =
    | Subscribe of int
    | Unsubscribe of int

type t = {
    conn: Postgresql.connection;
    clients: Clients.t;
    actions: action CCBlockingQueue.t;
    delay: float;
}

let make ?(n=32) ?(delay=0.25) conninfo clients = {
    conn = new Postgresql.connection ~conninfo ();
    clients;
    actions = CCBlockingQueue.create n;
    delay;
}

let handle_presence pubsub pt =
    Lwt_preemptive.run_in_main (fun () ->
        Lwt.return (
            let json = Server_message_converter.to_json (Presence pt) in
            Clients.game_send pubsub.clients pt.game_id json
        )
    )

let handle_notification pubsub payload =
    match Server_message_converter.of_json payload with
    | Presence (pt) -> handle_presence pubsub pt

let subscribe pubsub game_id =
    Subscribe game_id |>
    CCBlockingQueue.push pubsub.actions

let unsubscribe pubsub game_id =
    Unsubscribe game_id |>
    CCBlockingQueue.push pubsub.actions

let print_result (conn: Postgresql.connection) (result: Postgresql.result) =
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

let rec flush_result conn =
    match conn#get_result with
    | Some result -> (
        match result#status with
        | Postgresql.Command_ok -> flush_result conn
        | _ -> print_result conn result)
    | None -> ()

let rec hold pubsub f =
    let fd = Obj.magic pubsub.conn#socket in
    pubsub.conn#consume_input;
    if pubsub.conn#is_busy then
        let _ = Unix.select [fd] [] [] pubsub.delay in
        hold pubsub f
    else
        f ()

let get_notifications pubsub =
    hold pubsub (fun () ->
        let rec aux pubsub i =
            match pubsub.conn#notifies with
            | Some { Postgresql.Notification.name; pid; extra } ->
                handle_notification pubsub extra;
                aux pubsub (i + 1)
            | None -> i
        in aux pubsub 0
    )

let empty_query pubsub query =
    pubsub.conn#send_query @@ query;
    hold pubsub (fun () -> flush_result pubsub.conn)

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
            if i = 0 then Unix.sleepf pubsub.delay;
            aux ()
    in
    aux ()
