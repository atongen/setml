open Lwt
open Lwt.Infix

type t = {
    conn: Postgresql.connection;
    clients: Clients.t
}

let make conninfo clients = {
    conn = new Postgresql.connection ~conninfo ();
    clients
}

let ident x = x

module Option = struct
  type 'a t = 'a option
  let fold f = function None -> ident | Some x -> f x
end

module LwtUnix = struct
    type file_descr = Lwt_unix.file_descr

    let wrap_fd f fd = f (Lwt_unix.of_unix_file_descr fd)

    let poll ?(read = false) ?(write = false) ?timeout fd =
        let choices =
        [] |> (fun acc -> if read then Lwt_unix.wait_read fd :: acc else acc)
           |> (fun acc -> if write then Lwt_unix.wait_write fd :: acc else acc)
           |> Option.fold (fun t acc -> Lwt_unix.timeout t :: acc) timeout in
        if choices = [] then
            Lwt.fail_invalid_arg "Unix.poll: No operation specified."
        else
            begin
            Lwt.catch
                (fun () -> Lwt.choose choices >|= fun _ -> false)
                (function
                | Lwt_unix.Timeout -> Lwt.return_true
                | exn -> Lwt.fail exn)
            end >>= fun timed_out ->
            Lwt.return (Lwt_unix.readable fd, Lwt_unix.writable fd, timed_out)
end


let get_next_result (db: Postgresql.connection) =
    let aux fd =
        let rec hold () =
            db#consume_input;
            if db#is_busy then
                LwtUnix.poll ~read:true fd >|= (fun _ -> ()) >>= hold
            else
                return (Ok db#get_result) in
        (try hold () with
        | Postgresql.Error msg ->
            return (Error (msg))) in
    LwtUnix.wrap_fd aux (Obj.magic db#socket)

let get_result db =
    get_next_result db >>=
    (function
    | Ok None ->
        return (Error ("No response received after send"))
    | Ok (Some result) ->
        get_next_result db >>=
        (function
        | Ok None -> return (Ok result)
        | Ok (Some _) ->
            return (Error ("More than one message received"))
        | Error _ -> return (Error ("Unknown error 1")))
    | Error _ -> return (Error ("Unknown error 2")))

let subscribe pubsub game_id =
    let game_id_int = Util.int_of_base36 game_id in
    return (pubsub.conn#send_query @@ "listen game_" ^ string_of_int game_id_int ^ ";") >>=
    fun () -> get_result pubsub.conn

let unsubscribe pubsub game_id =
    let game_id_int = Util.int_of_base36 game_id in
    return (pubsub.conn#send_query @@ "unlisten game_" ^ string_of_int game_id_int ^ ";") >>=
    fun () -> get_result pubsub.conn

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