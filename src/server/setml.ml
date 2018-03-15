open Lwt
open Websocket
open Websocket_cohttp_lwt

open Lib

let clients = Clients.make ()
let id_counter = ref 0
let next_id () =
    id_counter := !id_counter + 1;
    !id_counter

let crypto = Crypto.make "super secret!"

let get_player_id crypto headers =
    match (Session.of_header crypto headers) with
    | Ok (session) -> Some(session.player_id)
    | Error (_) -> None

let handler
    (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
    (req  : Cohttp_lwt_unix.Request.t)
    (body : Cohttp_lwt.Body.t) =
    let open Frame in
    let uri = Cohttp.Request.uri req in
    let path = Uri.path uri in
    let meth = Cohttp.Request.meth req in
    let req_headers = Cohttp.Request.headers req in
    let headers = Cohttp.Header.init () in

    Lwt_io.eprintf "[CONN] %s\n%!" (Cohttp.Connection.to_string @@ snd conn)
    >>= fun _ ->
    Lwt_io.eprintf "[REQ] (%s,%s)\n%!" (Cohttp.Code.string_of_method meth) path
    >>= fun _ ->

    let open Route in
    match of_req req with
    | Game_create ->
        Cohttp_lwt_unix.Server.respond_redirect ~uri:(Uri.of_string "/games/abcd") ()
    | Game_show (game_id) -> begin
        match (Session.of_header crypto req_headers) with
        | Ok (session) -> begin
            Cohttp_lwt_unix.Server.respond_string
                ~status:`OK
                ~body:("<p>player_id: " ^ (string_of_int session.player_id) ^ ", game_id: " ^ game_id ^ "</p>")
                ()
            end
        | Error (exn) -> begin
            match exn with
            | Not_found -> begin
                let session = Session.make (next_id ()) in
                let cookie_key, header_val = Session.to_header session crypto in
                let headers = Cohttp.Header.add headers cookie_key header_val in
                Cohttp_lwt_unix.Server.respond_redirect ~headers ~uri:(Uri.of_string "/games/5") ()
                end
            | _ -> begin
                Cohttp_lwt_unix.Server.respond_string
                    ~status:`Internal_server_error
                    ~body:("<p>Error!</p>")
                    ()
                end
            end
        end
    | Ws_show (game_id) ->
        match get_player_id crypto req_headers with
        | Some(player_id) -> begin
            Cohttp_lwt.Body.drain_body body
            >>= fun () ->
            Websocket_cohttp_lwt.upgrade_connection req (fst conn) (
                fun f ->
                    match f.opcode with
                    | Opcode.Close ->
                        Printf.eprintf "[RECV] CLOSE\n%!"
                    | _ ->
                        Printf.eprintf "[RECV] %s: %s\n%!" (Websocket_cohttp_lwt.Frame.Opcode.to_string f.opcode) f.content;
                        Clients.game_send clients game_id "Player " ^ string_of_int player_id ^ ": " ^ f.content
            )
            >>= fun (resp, body, frames_out_fn) ->
            Clients.add clients game_id player_id frames_out_fn;
            Lwt.return (resp, (body :> Cohttp_lwt.Body.t))
        end
        | None -> begin
            Cohttp_lwt_unix.Server.respond_string
                ~status:`Internal_server_error
                ~body:("<p>Error!</p>")
                ()
        end
    | Static ->
        File_server.serve ~info:"Served by Cohttp/Lwt" ~docroot:"./public" ~index:"index.html" uri path
    | Not_found ->
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