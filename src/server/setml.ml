open Lwt
open Lwt.Infix
open Websocket
open Websocket_cohttp_lwt

open Lib

let clients = Clients.make ()
let crypto = Crypto.make "t8sK8LqFLn6Ixt9H6TMiS9HRs6BfcLyw6aXHi02omeOIp7mLYqSIlxtPgxXiETvpentbHMPkGYpiqW8nR9rJmeVU4aEEyzMbzDqIRznNSiqPnDb0Dp9PNerGuODpaeza"

let render_game game_id token =
    Cohttp_lwt_unix.Server.respond_string
        ~status:`OK
        ~body:(Templates.game game_id token)
        ()

let render_error msg =
    Cohttp_lwt_unix.Server.respond_error
        ~status:`Internal_server_error
        ~body:(Templates.error msg)
        ()

let render_not_found () = Cohttp_lwt_unix.Server.respond_not_found ()

let redirect ?headers uri =
    Cohttp_lwt_unix.Server.respond_redirect ?headers ~uri ()

let log msg = Lwt_io.printlf "%s" msg

(* render chain *)
let (>>=?) m f =
  m >>= function
  | Ok x -> f x
  | Error e ->
    Caqti_error.show e |> log >>= fun _ -> ();
    render_error "Oh no!"

let (>>=*) m f =
    m >>= function
    | Ok (x) -> f x
    | Error (err) -> Caqti_error.show err |> log

let make_handler db pubsub =
    fun (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
        (req  : Cohttp_lwt_unix.Request.t)
        (body : Cohttp_lwt.Body.t) ->
        let uri = Cohttp.Request.uri req in
        let path = Uri.path uri in
        let meth = Cohttp.Request.meth req in
        let req_headers = Cohttp.Request.headers req in
        let session = Session.of_header_or_new crypto req_headers in

        Lwt_io.eprintf "[CONN] %s\n%!" (Cohttp.Connection.to_string @@ snd conn)
        >>= fun _ ->
        Lwt_io.eprintf "[REQ] (%s,%s)\n%!" (Cohttp.Code.string_of_method meth) path
        >>= fun _ ->

        match Route.of_req req with
        | Route.Game_create ->
            Db.create_game db >>=? (fun game_id ->
            redirect (Route.game_show_uri game_id))
        | Route.Game_show (game_id) -> (
            match session.player_id with
            | Some (_) ->
                Db.game_exists db game_id >>=? (fun exists ->
                    if exists then (render_game game_id session.token) else render_not_found ()
                )
            | None ->
                Db.create_player db >>=? (fun player_id ->
                let headers = Session.set_player_id_headers session crypto player_id in
                redirect ~headers (Route.game_show_uri game_id)))
        | Route.Ws_show (game_id) -> (
            match session.player_id with
            | Some(player_id) -> (
                Cohttp_lwt.Body.drain_body body
                >>= fun () ->
                Websocket_cohttp_lwt.upgrade_connection req (fst conn) (
                    fun f ->
                        match f.opcode with
                        | Frame.Opcode.Close ->
                            (* websocket onclose *)
                            ignore (
                                Db.game_player_presence db game_id player_id false >>=* fun () ->
                                Clients.remove clients game_id player_id;
                                if not (Clients.game_has_players clients game_id) then Pubsub.unsubscribe pubsub game_id;
                                log ("Player " ^ (string_of_int player_id) ^ " left game " ^ string_of_int game_id);
                            )
                        | _ ->
                            (* websocket onmessage *)
                            Clients.game_send clients game_id ("From player " ^ (string_of_int player_id) ^ ": " ^ f.content))
                >>= fun (resp, body, frames_out_fn) ->
                (* websocket onopen *)
                Db.game_player_presence db game_id player_id true >>=? fun () ->
                ignore (log ("Player " ^ (string_of_int player_id) ^ " joined game " ^ string_of_int game_id));
                Clients.add clients game_id player_id frames_out_fn;
                Pubsub.subscribe pubsub game_id;
                Lwt.return (resp, (body :> Cohttp_lwt.Body.t)))
            | None -> render_error "Unable to get player id from session!")
        | Route.Static ->
            File_server.serve ~info:"Served by Cohttp/Lwt" ~docroot:"./public" ~index:"index.html" uri path
        | Route.Route_not_found -> render_not_found ()

let start_server host port () =
    let conn_closed (ch,_) =
        Printf.eprintf "[SERV] connection %s closed\n%!"
        (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
    in
    Lwt_io.eprintf "[SERV] Listening for HTTP on port %d\n%!" port >>= fun () ->
    Caqti_lwt.connect (Uri.of_string "postgresql://atongen:at1234@localhost:5435/setml_development") >>= function
    | Ok db ->
        let pubsub = Pubsub.make "user=atongen password=at1234 port=5435 host=localhost dbname=setml_development" clients in
        ignore (Lwt_preemptive.detach Pubsub.start pubsub);
        Cohttp_lwt_unix.Server.create
            ~mode:(`TCP (`Port port))
            (Cohttp_lwt_unix.Server.make ~callback:(make_handler db pubsub) ~conn_closed ())
    | Error err ->
        Lwt.return (print_endline ("Failed to connect to db!"))

let () =
    let random_generator = Nocrypto.Rng.create (module Nocrypto.Rng.Generators.Fortuna) in
    Nocrypto_entropy_unix.initialize ();
    Nocrypto_entropy_unix.reseed random_generator;

    let port = if Array.length Sys.argv = 2 then
        int_of_string Sys.argv.(1)
    else 7777
    in
    Lwt_main.run (start_server "localhost" port ())