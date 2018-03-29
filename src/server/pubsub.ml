open Lwt
open Lwt.Infix

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
    Clients.game_send pubsub.clients game_id ("Player " ^ string_of_int player_id ^ " " ^ msg ^ " the game.")

let handle_notification pubsub payload =
    ignore (print_endline @@ "handle_notification: " ^ payload);
    match Yojson.Safe.from_string payload with
    | `Assoc ["type", `String "present"; "game_id", `Int game_id_int; "player_id", `Int player_id; "value", `Bool present] ->
        let game_id = Util.base36_of_int game_id_int in
        handle_present pubsub game_id player_id present
    | _ -> ()

let poll ?(read = false) ?(write = false) ?timeout fd =
    let ident x = x in
    let fold f = function None -> ident | Some x -> f x in
    let choices =
    [] |> (fun acc -> if read then Lwt_unix.wait_read fd :: acc else acc)
        |> (fun acc -> if write then Lwt_unix.wait_write fd :: acc else acc)
        |> fold (fun t acc -> Lwt_unix.timeout t :: acc) timeout in
    if choices = [] then
        Lwt.fail_invalid_arg "poll: No operation specified."
    else
        begin
        Lwt.catch
            (fun () -> Lwt.choose choices >|= fun _ -> false)
            (function
            | Lwt_unix.Timeout -> Lwt.return_true
            | exn -> Lwt.fail exn)
        end >>= fun timed_out ->
        Lwt.return (Lwt_unix.readable fd, Lwt_unix.writable fd, timed_out)


let get_next_result (conn: Postgresql.connection) =
    let wrap_fd f fd = f (Lwt_unix.of_unix_file_descr fd) in
    let aux fd =
        let rec hold () =
            conn#consume_input;
            if conn#is_busy then
                poll ~read:true fd >|= (fun _ -> ()) >>= hold
            else
                return (Ok conn#get_result) in
        (try hold () with
        | Postgresql.Error msg -> return (Error (msg))) in
    wrap_fd aux (Obj.magic conn#socket)

let get_result conn =
    get_next_result conn >>=
    (function
    | Ok None ->
        return (Error ("No response received after send"))
    | Ok (Some _) ->
        get_next_result conn >>=
        (function
        | Ok None -> return (Ok ())
        | Ok (Some _) ->
            return (Error ("More than one message received"))
        | Error e -> return (Error (Postgresql.string_of_error e)))
    | Error e -> return (Error (Postgresql.string_of_error e)))

let get_next_notification pubsub =
    let wrap_fd f fd = f (Lwt_unix.of_unix_file_descr fd) in
    let aux fd =
        let rec hold () =
            pubsub.conn#consume_input;
            if pubsub.conn#is_busy then
                poll ~read:true fd >|= (fun _ -> ()) >>= hold
            else
                match pubsub.conn#notifies with
                | Some { extra } -> return_ok (handle_notification pubsub extra)
                | None -> return_ok ()
                in
        (try hold () with
        | Postgresql.Error e -> return_error (Postgresql.string_of_error e)) in
    wrap_fd aux (Obj.magic pubsub.conn#socket)

let subscribe pubsub game_id =
    Lwt.return (
        let game_id_int = Util.int_of_base36 game_id in
        let action = Subscribe game_id_int in
        CCBlockingQueue.push pubsub.actions action
    )

let unsubscribe pubsub game_id =
    Lwt.return (
        let game_id_int = Util.int_of_base36 game_id in
        let action = Unsubscribe game_id_int in
        CCBlockingQueue.push pubsub.actions action
    )

let handle_action pubsub = function
    | Subscribe (game_id_int) ->
        return (pubsub.conn#send_query @@ "listen game_" ^ string_of_int game_id_int ^ ";") >>=
        fun () -> get_result pubsub.conn
    | Unsubscribe (game_id_int) ->
        return (pubsub.conn#send_query @@ "unlisten game_" ^ string_of_int game_id_int ^ ";") >>=
        fun () -> get_result pubsub.conn
let start pubsub =
    let rec aux () =
        match CCBlockingQueue.try_take pubsub.actions with
        | Some (a) -> handle_action pubsub a >>= fun _ -> aux ()
        | None -> get_next_notification pubsub >>= fun _ -> aux ()
    in
    aux ()

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