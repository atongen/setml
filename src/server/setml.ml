open Lwt
open Lwt.Infix
open Websocket
open Websocket_cohttp_lwt

open Lib

let clients = Clients.make ()
let crypto = Crypto.make "t8sK8LqFLn6Ixt9H6TMiS9HRs6BfcLyw6aXHi02omeOIp7mLYqSIlxtPgxXiETvpentbHMPkGYpiqW8nR9rJmeVU4aEEyzMbzDqIRznNSiqPnDb0Dp9PNerGuODpaeza"

let session_player_id crypto headers =
    match (Session.of_header crypto headers) with
    | Ok (session) -> Some(session.player_id)
    | Error (_) -> None

let expiration = `Max_age (Int64.of_int (10 * 365 * 24 * 60 * 60))

let render_game game_id =
    Cohttp_lwt_unix.Server.respond_string
        ~status:`OK
        ~body:(Templates.game game_id)
        ()

let render_error msg =
    Cohttp_lwt_unix.Server.respond_string
        ~status:`Internal_server_error
        ~body:(Templates.error msg)
        ()

let render_not_found msg =
    Cohttp_lwt_unix.Server.respond_string
        ~status:`Not_found
        ~body:msg
        ()

let redirect ?headers uri =
    Cohttp_lwt_unix.Server.respond_redirect ?headers ~uri:(Uri.of_string uri) ()

let make_handler db =
    fun (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
        (req  : Cohttp_lwt_unix.Request.t)
        (body : Cohttp_lwt.Body.t) ->
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
            Db.create_game db >>= (function
            | Ok game_id_opt -> (
                match game_id_opt with
                | Some (game_id_int) ->
                    let game_id = Util.base36_of_int game_id_int in
                    ignore (print_endline ("Creating game " ^ game_id));
                    redirect ("/games/" ^ game_id)
                | None -> render_error "Unable to create game!")
            | Error e -> render_error (Caqti_error.show e))
        | Route.Game_show (game_id) -> (
            match (Session.of_header crypto req_headers) with
            | Ok (session) -> render_game game_id
            | Error (_) -> (
                Db.create_player db >>= (function
                    | Ok player_id_opt -> (
                        match player_id_opt with
                        | Some (player_id) ->
                            let session = Session.make (player_id) in
                            let cookie_key, header_val = Session.to_header ~expiration ~path:"/" session crypto in
                            let headers = Cohttp.Header.add headers cookie_key header_val in
                            redirect ~headers ("/games/" ^ game_id)
                        | None -> render_error "Unable to create player!")
                    | Error e -> render_error (Caqti_error.show e))))
        | Route.Ws_show (game_id) -> (
            match session_player_id crypto req_headers with
            | Some(player_id) -> (
                Cohttp_lwt.Body.drain_body body
                >>= fun () ->
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
                            Clients.game_send clients game_id ("From player " ^ (string_of_int player_id) ^ ": " ^ f.content))
                >>= fun (resp, body, frames_out_fn) ->
                ignore (print_endline ("Adding player " ^ (string_of_int player_id) ^ " to game " ^ game_id));
                Clients.add clients game_id player_id frames_out_fn;
                Clients.game_send clients game_id ("Player " ^ string_of_int player_id ^ " joined!");
                Lwt.return (resp, (body :> Cohttp_lwt.Body.t)))
            | None -> render_error "Unable to get player id from session!")
        | Route.Static ->
            File_server.serve ~info:"Served by Cohttp/Lwt" ~docroot:"./public" ~index:"index.html" uri path
        | Route.Route_not_found -> render_not_found (Sexplib.Sexp.to_string_hum (Cohttp.Request.sexp_of_t req))

let start_server host port () =
  let conn_closed (ch,_) =
    Printf.eprintf "[SERV] connection %s closed\n%!"
      (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
  in
  Lwt_io.eprintf "[SERV] Listening for HTTP on port %d\n%!" port >>= fun () ->
  Caqti_lwt.connect (Uri.of_string "postgresql://atongen:at1234@localhost:5435/setml_development") >>= function
  | Ok db ->
    Cohttp_lwt_unix.Server.create
        ~mode:(`TCP (`Port port))
        (Cohttp_lwt_unix.Server.make ~callback:(make_handler db) ~conn_closed ())
  | Error err ->
    Lwt.return (print_endline ("Failed to connect to db!"))

let () =
  Lwt_main.run (start_server "localhost" 7777 ())