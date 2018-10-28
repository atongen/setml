module Listen_set = CCSet.Make(CCInt)

type action =
    | Subscribe of int
    | Unsubscribe of int

type t = {
    conn: Postgresql.connection;
    clients: Clients.t;
    actions: action CCBlockingQueue.t;
    mutable listening: Listen_set.t;
}

let make ?(n=32) ?(retries=0) conninfo clients =
    let rec aux attempt max =
        try
            let conn = new Postgresql.connection ~conninfo () in
            Ok {
                conn;
                clients;
                actions = CCBlockingQueue.create n;
                listening = Listen_set.empty;
            }
        with Postgresql.Error e ->
            let msg = Postgresql.string_of_error e in
            if attempt < max then (
                ignore(print_endline (Printf.sprintf "Error connecting to pubsub db with '%s', retrying..." conninfo));
                Unix.sleep 1;
                aux (attempt + 1) max
            ) else (
                Error msg
            )
    in aux 0 retries

let channel_game_id_re = Re.Pcre.regexp "^game_([0-9]+)$"

let channel_game_id channel =
    let result = Re.Pcre.extract ~rex:channel_game_id_re channel in
    if Array.length result = 2 then
        Some(int_of_string result.(1))
    else None

let handle_notifications pubsub msgs =
    List.iter (fun msg ->
        let channel = msg.Postgresql.Notification.name in
        match channel_game_id channel with
        | Some (game_id) ->
            Lwt_preemptive.run_in_main (fun () ->
                Lwt.return (
                    Clients.game_send pubsub.clients game_id msg.extra
                )
            )
        | None -> ignore (print_endline ("Unknown channel: " ^ channel ^ " from pid " ^ string_of_int msg.pid))
    ) msgs

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

let rec hold ?(delay=1.0) pubsub f =
    let fd = Obj.magic pubsub.conn#socket in
    pubsub.conn#consume_input;
    if pubsub.conn#is_busy then (
        let _ = Unix.select [fd] [] [] delay in
        hold pubsub f
    ) else (
        f ())

let get_notifications pubsub =
    hold pubsub (fun () ->
        let rec aux pubsub acc =
            match pubsub.conn#notifies with
            | Some (n) -> aux pubsub (n :: acc)
            | None -> acc
        in aux pubsub [])
    |> List.rev

let empty_query pubsub query =
    pubsub.conn#send_query @@ query;
    hold pubsub (fun () -> flush_result pubsub.conn)

let subscribe pubsub game_id =
    Subscribe game_id |>
    CCBlockingQueue.push pubsub.actions

let unsubscribe pubsub game_id =
    Unsubscribe game_id |>
    CCBlockingQueue.push pubsub.actions

let handle_subscribe pubsub game_id =
    if not (Listen_set.mem game_id pubsub.listening) then
        empty_query pubsub ("listen game_" ^ string_of_int game_id ^ ";");
        pubsub.listening <- Listen_set.add game_id pubsub.listening

let handle_unsubscribe pubsub game_id =
    if Listen_set.mem game_id pubsub.listening then
        empty_query pubsub ("unlisten game_" ^ string_of_int game_id ^ ";");
        pubsub.listening <- Listen_set.remove game_id pubsub.listening

let handle_action pubsub action =
    match action with
    | Subscribe (game_id) -> handle_subscribe pubsub game_id
    | Unsubscribe (game_id) -> handle_unsubscribe pubsub game_id

let action_handler pubsub =
    while true do
        let action = CCBlockingQueue.take pubsub.actions in
        handle_action pubsub action
    done

let start pubsub =
    let _ = Thread.create action_handler pubsub in
    let rec aux () =
        let msgs = get_notifications pubsub in
        if List.length msgs > 0 then (
            handle_notifications pubsub msgs
        ) else (
            Unix.sleepf 0.1
        );
        aux ()
    in
    aux ()
