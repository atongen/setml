open Lwt
open Websocket
open Websocket_cohttp_lwt

let clients = CCVector.create ()

let handler
    (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
    (req  : Cohttp_lwt_unix.Request.t)
    (body : Cohttp_lwt.Body.t) =
  let open Frame in
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  let meth = Cohttp.Request.meth req in
  let headers = Cohttp.Request.headers req in
  Lwt_io.eprintf "[CONN] %s\n%!" (Cohttp.Connection.to_string @@ snd conn)
  >>= fun _ ->
  Lwt_io.eprintf "[REQ] (%s,%s)\n%!" (Cohttp.Code.string_of_method meth) path
  >>= fun _ ->
  match (meth, path) with
  | (`GET, "/ws") ->
    Cohttp_lwt.Body.drain_body body
    >>= fun () ->
    Websocket_cohttp_lwt.upgrade_connection req (fst conn) (
        fun f ->
            match f.opcode with
            | Opcode.Close ->
                Printf.eprintf "[RECV] CLOSE\n%!"
            | _ ->
                Printf.eprintf "[RECV] %s: %s\n%!" (Websocket_cohttp_lwt.Frame.Opcode.to_string f.opcode) f.content;
                CCVector.iter begin fun send ->
                    Lwt.ignore_result (Lwt.wrap1 send @@ Some (Frame.create ~content:f.content ()))
                end clients
    )
    >>= fun (resp, body, frames_out_fn) ->
    CCVector.push clients frames_out_fn;
    Lwt.return (resp, (body :> Cohttp_lwt.Body.t))
  | (`GET, _) ->
    File_server.serve ~info:"Served by Cohttp/Lwt" ~docroot:"./public" ~index:"index.html" uri path
  | (_, _) ->
    Cohttp_lwt_unix.Server.respond_string
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

(* main *)
let () =
    Lwt_main.run (start_server "localhost" 7777 ())
