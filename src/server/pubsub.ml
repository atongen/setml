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

let channel_game_id_re = Re.Pcre.regexp "^game_([0-9]+)$"

let channel_game_id channel =
    let result = Re.Pcre.extract ~rex:channel_game_id_re channel in
    if Array.length result = 2 then
        Some(int_of_string result.(1))
    else None

let handle_notification pubsub channel pid payload =
    match channel_game_id channel with
    | Some (game_id) ->
        Lwt_preemptive.run_in_main (fun () ->
            Lwt.return (
                Clients.game_send pubsub.clients game_id payload
            )
        )
    | None -> ignore (print_endline ("Unknown channel: " ^ channel ^ " from pid " ^ string_of_int pid))

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

let get_notifications pubsub f =
  hold pubsub (fun () ->
      let rec aux pubsub i =
        match pubsub.conn#notifies with
        | Some { Postgresql.Notification.name; pid; extra } ->
          f pubsub name pid extra;
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
      let i = get_notifications pubsub handle_notification in
      if i = 0 then Unix.sleepf pubsub.delay;
      aux ()
  in
  aux ()
