open Lwt
open Websocket
open Websocket_cohttp_lwt

module Session = struct
  module Backend = struct
    include Session.Lift.IO(Lwt)(Session.Memory)
    let create () = Session.Memory.create ()
  end
  include Session_cohttp_lwt.Make(Backend)

  let increment t session =
    let value = string_of_int (1 + int_of_string session.value) in
    set t ~value session
end

let clients = CCVector.create ()
let cookie = "__setml_session"
let session_store = Session.Backend.create ()

let handler
    (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
    (req  : Cohttp_lwt_unix.Request.t)
    (body : Cohttp_lwt.Body.t) =
  let open Frame in
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  let meth = Cohttp.Request.meth req in
  let req_headers = Cohttp.Request.headers req in
  Lwt_io.eprintf "[CONN] %s\n%!" (Cohttp.Connection.to_string @@ snd conn)
  >>= fun _ ->
  Lwt_io.eprintf "[REQ] (%s,%s)\n%!" (Cohttp.Code.string_of_method meth) path
  >>= fun _ ->
  Session.of_header_or_create session_store cookie "0" req_headers >>= fun session ->
    Session.increment session_store session >>= fun () ->
    let headers = Cohttp.Header.of_list (Session.to_cookie_hdrs cookie session) in
    let header_val = session.Session.value in
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
                    Lwt.ignore_result (Lwt.wrap1 send @@ Some (Frame.create ~content:(header_val ^ ": " ^ f.content) ()))
                end clients
    )
    >>= fun (resp, body, frames_out_fn) ->
    CCVector.push clients frames_out_fn;
    Lwt.return (resp, (body :> Cohttp_lwt.Body.t))
  | (`GET, _) ->
    File_server.serve ~info:"Served by Cohttp/Lwt" ~docroot:"./public" ~index:"index.html" ~headers:headers uri path
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
    Nocrypto_entropy_lwt.initialize () |> ignore;
    Lwt_main.run (start_server "localhost" 7777 ())
