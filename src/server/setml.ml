open Lwt
open Websocket
open Websocket_cohttp_lwt

open Lib

let clients = Clients.make ()
let id_counter = ref 0
let next_id () =
    ignore (print_endline ("before id_counter: " ^ (string_of_int !id_counter)));
    id_counter := !id_counter + 1;
    ignore (print_endline ("after id_counter: " ^ (string_of_int !id_counter)));
    !id_counter

let crypto = Crypto.make "t8sK8LqFLn6Ixt9H6TMiS9HRs6BfcLyw6aXHi02omeOIp7mLYqSIlxtPgxXiETvpentbHMPkGYpiqW8nR9rJmeVU4aEEyzMbzDqIRznNSiqPnDb0Dp9PNerGuODpaeza"

let expiration = `Max_age (Int64.of_int (3 * 365 * 24 * 60 * 60))

let get_player_id crypto headers =
    match (Session.of_header crypto headers) with
    | Ok (session) ->
        ignore (print_endline ("Decoded player id " ^ (string_of_int session.player_id) ^ " from cookie"));
        Some(session.player_id)
    | Error (_) -> None

let handler
    (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
    (req  : Cohttp_lwt_unix.Request.t)
    (body : Cohttp_lwt.Body.t) =
    let uri = Cohttp.Request.uri req in
    let path = Uri.path uri in
    let meth = Cohttp.Request.meth req in
    let req_headers = Cohttp.Request.headers req in
    let headers = Cohttp.Header.init () in

    Lwt_io.eprintf "[CONN] %s\n%!" (Cohttp.Connection.to_string @@ snd conn)
    >>= fun _ ->
    Lwt_io.eprintf "[REQ] (%s,%s)\n%!" (Cohttp.Code.string_of_method meth) path
    >>= fun _ ->

    match Route.of_req req with
    | Route.Game_create ->
        Cohttp_lwt_unix.Server.respond_redirect ~uri:(Uri.of_string "/games/abcd") ()
    | Route.Game_show (game_id) -> (
        match (Session.of_header crypto req_headers) with
        | Ok (session) ->
            ignore (print_endline ("Found cookie for player: " ^ Session.to_json session));
            Cohttp_lwt_unix.Server.respond_string
                ~status:`OK
                ~body:(Templates.game game_id)
                ()
        | Error (_) -> begin
            let my_id = next_id () in
            ignore (print_endline ("Got next id: " ^ string_of_int my_id));
            let session = Session.make (my_id) in
            let cookie_key, header_val = Session.to_header ~expiration ~path:"/" session crypto in
            let headers = Cohttp.Header.add headers cookie_key header_val in
            ignore (print_endline ("Setting cookie for player " ^ Session.to_json session ^ ", redirecting"));
            Cohttp_lwt_unix.Server.respond_redirect ~headers ~uri:(Uri.of_string ("/games/" ^ game_id)) ()
        end
    )
    | Route.Ws_show (game_id) -> (
        match get_player_id crypto req_headers with
        | Some(player_id) -> begin
            Cohttp_lwt.Body.drain_body body
            >>= fun () ->
            ignore (print_endline ("Establish websocket for player " ^ string_of_int player_id ^ " in game " ^ game_id));
            Websocket_cohttp_lwt.upgrade_connection req (fst conn) (
                fun f ->
                    match f.opcode with
                    | Frame.Opcode.Close ->
                        Printf.eprintf "[RECV] CLOSE\n%!";
                        ignore (print_endline ("Removing player " ^ (string_of_int player_id) ^ " from game " ^ game_id));
                        Clients.remove clients game_id player_id;
                        Clients.game_send clients game_id ("Player " ^ string_of_int player_id ^ " left!")
                    | _ ->
                        Printf.eprintf "[RECV] %s: %s\n%!" (Websocket_cohttp_lwt.Frame.Opcode.to_string f.opcode) f.content;
                        ignore (print_endline ("Sending from player " ^ (string_of_int player_id) ^ " to game " ^ game_id));
                        Clients.game_send clients game_id ("From player " ^ (string_of_int player_id) ^ ": " ^ f.content)
            )
            >>= fun (resp, body, frames_out_fn) ->
            ignore (print_endline ("Adding player " ^ (string_of_int player_id) ^ " to game " ^ game_id));
            Clients.add clients game_id player_id frames_out_fn;
            Clients.game_send clients game_id ("Player " ^ string_of_int player_id ^ " joined!");
            Lwt.return (resp, (body :> Cohttp_lwt.Body.t))
        end
        | None -> begin
            Cohttp_lwt_unix.Server.respond_string
                ~status:`Internal_server_error
                ~body:("<p>Error!</p>")
                ()
        end
    )
    | Route.Static ->
        File_server.serve ~info:"Served by Cohttp/Lwt" ~docroot:"./public" ~index:"index.html" uri path
    | Route.Route_not_found ->
        Cohttp_lwt_unix.Server.respond_string
            ~headers:headers
            ~status:`Not_found
            ~body:(Sexplib.Sexp.to_string_hum (Cohttp.Request.sexp_of_t req))
            ()

let start_server host port () =
  let conn_closed (ch,_) =
    Printf.eprintf "[SERV] connection %s closed\n%!"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  Lwt_io.eprintf "[SERV] Listening for HTTP on port %d\n%!" port >>= fun () ->
  Cohttp_lwt_unix.Server.create
    ~mode:(`TCP (`Port port))
    (Cohttp_lwt_unix.Server.make ~callback:handler ~conn_closed ())

let () =
  Lwt_main.run (start_server "localhost" 7777 ())