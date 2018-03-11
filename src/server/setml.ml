open Lwt
open Websocket
open Websocket_cohttp_lwt

open Lib

(*let clients = CCVector.create () *)
let cookie_key = "__setml_session"
let session_default = "0"
let id_counter = ref 0
let next_id () =
    id_counter := !id_counter + 1;
    string_of_int !id_counter

let session = Crypto.make "super secret!"

let handler
    (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
    (req  : Cohttp_lwt_unix.Request.t)
    (body : Cohttp_lwt.Body.t) =
  let open Frame in
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  let meth = Cohttp.Request.meth req in
  let req_headers = Cohttp.Request.headers req in

  (*
  Sess.of_header_or_create session_backend session_default cookie_key req_headers >>= fun session ->
  if session.Sess.value = session_default then
    Sess.set ~value:(next_id ()) session_backend
  else
  let headers = Cohttp.Header.of_list (Sess.to_cookie_hdrs cookie_key session) in
  let header_val = session.Sess.value in
  *)
  let headers = Cohttp.Header.of_list [] in

  Lwt_io.eprintf "[CONN] %s\n%!" (Cohttp.Connection.to_string @@ snd conn)
  >>= fun _ ->
  Lwt_io.eprintf "[REQ] (%s,%s)\n%!" (Cohttp.Code.string_of_method meth) path
  >>= fun _ ->
  Lwt_io.eprintf "[HEADERS] %s\n%!" (Cohttp.Header.to_string req_headers)
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
                (*
                CCVector.iter begin fun send ->
                    Lwt.ignore_result (Lwt.wrap1 send @@ Some (Frame.create ~content:(header_val ^ ": " ^ f.content) ()))
                end clients
                *)
    )
    >>= fun (resp, body, frames_out_fn) ->
    (*CCVector.push clients frames_out_fn; *)
    Lwt.return (resp, (body :> Cohttp_lwt.Body.t))
  | (`GET, "/ok") -> begin
    let input = "abcd1234abcd1234efgh5678efgh5678 non 16" in
    let enc = Crypto.encrypt_and_sign session input in
    match Crypto.verify_and_decrypt session enc with
    | Ok(_, dec) -> begin
        Cohttp_lwt_unix.Server.respond_string
            ~headers:headers
            ~status:`OK
            ~body:("<p>input: " ^ input ^ ",<br/>enc: " ^ enc ^ ",<br/>dec: " ^ dec ^ "</p>")
            ()
      end
    | Error(msg) -> begin
        Cohttp_lwt_unix.Server.respond_string
            ~headers:headers
            ~status:`OK
            ~body:("<p>input: " ^ input ^ ",<br/>enc: " ^ enc ^ ",<br/>error: " ^ msg ^ "</p>")
            ()

      end
    end
  | (`GET, _) ->
    File_server.serve ~info:"Served by Cohttp/Lwt" ~docroot:"./public" ~index:"index.html" ~headers:headers uri path
  | (_, _) ->
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