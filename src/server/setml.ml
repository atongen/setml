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
    )
    >>= fun (resp, body, frames_out_fn) ->
    (* send a message to the client every second *)
    let _ =
        let num_ref = ref 10 in
        let rec go () =
            if !num_ref > 0 then
                let msg = Printf.sprintf "-> Ping %d" !num_ref in
                Lwt_io.eprintf "[SEND] %s\n%!" msg
                >>= fun () ->
                Lwt.wrap1 frames_out_fn @@
                    Some (Frame.create ~content:msg ())
                >>= fun () ->
                Lwt.return (num_ref := !num_ref - 1)
                >>= fun () ->
                Lwt_unix.sleep 1.
                >>= go
            else
                Lwt_io.eprintf "[INFO] Test done\n%!"
                >>= Lwt.return
        in
        go ()
    in
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
