open Lwt.Infix
open Websocket_cohttp_lwt

open Lib

let render_index ~headers ~player_id token manifest info =
  Cohttp_lwt_unix.Server.respond_string
    ~headers
    ~status:`OK
    ~body:(Templates.index_page ~player_id ~title:"SetML" ~token ~manifest ~info)
    ()

let render_game ~headers ~player_id game_id token manifest info =
  let game_id_str = Shared.Base_conv.base36_of_int game_id in
  Cohttp_lwt_unix.Server.respond_string
    ~headers
    ~status:`OK
    ~body:(Templates.game_page ~player_id ~title:("SetML: " ^ game_id_str) ~token ~manifest ~info)
    ()

let render_content ~content_type content =
  let headers = Cohttp.Header.init_with "Content-type" content_type in
  Cohttp_lwt_unix.Server.respond_string
    ~status:`OK
    ~headers
    ~body:content
    ()

let render_error ?headers msg =
  Cohttp_lwt_unix.Server.respond_error
    ?headers
    ~status:`Internal_server_error
    ~body:(Templates.error msg)
    ()

let render_forbidden () =
  Cohttp_lwt_unix.Server.respond_error
    ~status:`Forbidden
    ~body:(Templates.error "Forbidden!")
    ()

let render_not_found () = Cohttp_lwt_unix.Server.respond_not_found ()

let redirect ?headers uri =
  Cohttp_lwt_unix.Server.respond_redirect ?headers ~uri ()

let log msg = Lwt_io.printlf "%s" msg

(* render chain *)
let (>>=?) m f =
    m >>= function
    | Ok x -> f x
    | Error _ ->
        render_error "Oh no!"

let (>>=*) m f =
    m >>= function
    | Ok x -> f x
    | Error e ->
        let msg = Caqti_error.show e in
        log msg

let handle_message pool game_id player_id player_token json =
    let msg = Server_messages.Server_message_converter.of_json json in
    match msg with
    | Server_game _ | Server_player _ | Server_name _ | Server_card _ | Server_board_card _ | Server_game_update _
    | Server_score _ | Server_move _ | Server_presence _ | Server_move_info _ | Server_shuffles _ ->
        log "Server message recieved from client!"
    | Client_start_game in_token ->
        if in_token <> player_token then Lwt.return_unit else
        Db.start_game ~game_id pool >>=* fun () ->
        Lwt.return_unit
    | Client_move (in_token, d) ->
        if in_token <> player_token then Lwt.return_unit else
        Db.create_move ~game_id ~player_id ~cards:(d.card0, d.card1, d.card2) pool >>=* fun _idx ->
        Db.is_game_over ~game_id pool >>=* fun is_over ->
        if is_over then
            Db.end_game ~game_id pool >>=* fun () ->
            Lwt.return_unit
        else
            Lwt.return_unit
    | Client_shuffle in_token ->
        if in_token <> player_token then Lwt.return_unit else
        Db.create_shuffle ~game_id ~player_id pool >>=* fun _ ->
        Lwt.return_unit
    | Client_name (in_token, name) ->
        if in_token <> player_token then Lwt.return_unit else
        Db.update_player_name ~player_id ~name pool >>=* fun _ ->
        Lwt.return_unit
    | Client_theme (in_token, theme) ->
        if in_token <> player_token then Lwt.return_unit else
        Db.update_game_theme ~game_id ~theme pool >>=* fun _ ->
        Lwt.return_unit

let get_manifest docroot =
    let f = docroot ^ "/assets/manifest.json" in
    if Sys.file_exists f then
        let open Yojson.Basic in
        let json = from_file f in
        List.map (fun (key, json_value) ->
            (key, Util.to_string json_value)
        ) (Util.to_assoc json)
    else []

let make_handler pool pubsub clients crypto docroot =
    let manifest = get_manifest docroot in
    let info = Info.to_string (Info.get ()) in
    fun (conn : Conduit_lwt_unix.flow * Cohttp.Connection.t)
        (req  : Cohttp_lwt_unix.Request.t)
        (body : Cohttp_lwt.Body.t) ->

        let uri = Cohttp.Request.uri req in
        let path = Uri.path uri in
        let meth = Cohttp.Request.meth req in
        let req_headers = Cohttp.Request.headers req in
        let session = Session.of_header_or_new crypto req_headers in
        let headers = Session.to_headers session crypto in

        Lwt_io.eprintf "[REQ] (%s,%s)\n%!" (Cohttp.Code.string_of_method meth) path
        >>= fun _ ->

        match Route.of_req req with
        | Route.Index -> render_index ~player_id:session.player_id ~headers session.token manifest info
        | Game_create -> (
            Cohttp_lwt.Body.to_string body >>= fun my_body ->
            match Server_util.form_value my_body "token" with
            | Some (token) ->
                if session.token = token then (
                    match Server_util.form_value_int_of_base36 my_body "previous_game_id" with
                    | Some previous_game_id ->
                        Db.create_game_from_previous ~game_id:previous_game_id pool >>=? fun game_id ->
                        redirect ~headers (Route.game_show_uri game_id)
                    | None ->
                        Db.create_game pool >>=? fun game_id ->
                        redirect ~headers (Route.game_show_uri game_id)
                ) else render_forbidden ()
            | None -> render_forbidden ()
        )
        | Game_show game_id -> (
            let create_player_and_redirect () =
                Db.create_player pool >>=? fun player_id ->
                let headers = Session.set_player_id_headers session crypto player_id in
                redirect ~headers (Route.game_show_uri game_id)
            in
            match session.player_id with
            | Some player_id ->
                Db.player_exists ~player_id pool >>=? fun player_exists ->
                if player_exists then (
                    Db.can_join ~game_id ~player_id pool >>=? fun can_join ->
                    if can_join then (
                        render_game ~player_id:(Some player_id) ~headers game_id session.token manifest info
                    ) else (
                        render_not_found ()
                    )
                ) else (
                    create_player_and_redirect ()
                )
            | None -> create_player_and_redirect ()
        )
        | Ws_show game_id -> (
            match session.player_id with
            | Some player_id -> (
                Db.player_exists ~player_id pool >>=? fun player_exists ->
                if player_exists then (
                    Db.can_join ~game_id ~player_id pool >>=? fun can_join ->
                    if can_join then (
                        Cohttp_lwt.Body.drain_body body >>= fun () ->
                        Websocket_cohttp_lwt.upgrade_connection req (fst conn) (fun f -> (
                            match f.opcode with
                            | Frame.Opcode.Close ->
                                (* websocket onclose *)
                                Clients.remove clients game_id player_id;
                                if not (Clients.game_has_players clients game_id) then Pubsub.unsubscribe pubsub game_id;
                                Db.set_game_player_presence ~game_id ~player_id ~present:false pool >>=* fun () ->
                                Lwt.return_unit
                            | Frame.Opcode.Text ->
                                (* websocket onmessage *)
                                handle_message pool game_id player_id session.token f.content >>= fun () ->
                                Lwt.return_unit
                            | e -> Printf.sprintf "Unhandled websocket frame opcode: %s" (Frame.Opcode.to_string e) |> log
                            ) |> ignore
                        )
                        >>= fun (resp, body, frames_out_fn) ->
                        (* websocket onopen *)
                        Pubsub.subscribe pubsub game_id;
                        Clients.add clients game_id player_id frames_out_fn;
                        Db.set_game_player_presence ~game_id ~player_id ~present:true pool >>=? fun () ->
                        Lwt.return (resp, (body :> Cohttp_lwt.Body.t))
                    ) else (
                        render_not_found ()
                    )
                ) else (
                    render_not_found ()
                )
            )
            | None -> render_error "Unable to get player id from session!"
        )
        | Player_games -> (
            match session.player_id with
            | Some player_id ->
                let open Server_api_messages.Server_api_message_converter in
                Db.find_player_games ~player_id pool >>=? fun player_games ->
                let content = player_games_to_json player_games in
                render_content ~content_type:"application/json" content
            | None -> render_content ~content_type:"application/json" "[]"
        )
        | CardSvg (theme, card) ->
            let content = Shared.Theme.make_card_svg ~width:300.0 ~height:300.0 ~theme card in
            render_content ~content_type:"image/svg+xml" content
        | Static ->
            File_server.serve ~info:"SetML File Server" ~docroot ~index:"index.html" uri path
        | Route_not_found -> render_not_found ()

let start_server (config: Config.t) =
    let conn_closed (ch,_) =
        Printf.eprintf "Connection %s closed\n%!"
        (Sexplib.Sexp.to_string_hum (Conduit_lwt_unix.sexp_of_flow ch))
    in
    let crypto = Crypto.make config.crypto_secret in
    let clients = Clients.make () in
    match Pubsub.make ~retries:60 (Config.db_conninfo config) clients with
    | Ok pubsub -> (
        ignore (Lwt_preemptive.detach Pubsub.start pubsub);
        Db.make ~max_size:config.db_pool (Config.db_uri_str config) >>= function
        | Ok pool ->
            (* https://stackoverflow.com/questions/40497364/lwt-and-cohttp-fatal-error-exception-unix-unix-errorunix-econnreset-read *)
            Lwt.async_exception_hook := (function
                | Unix.Unix_error (error, func, arg) ->
                    Logs.warn (fun m ->
                    m  "Client connection error %s: %s(%S)"
                        (Unix.error_message error) func arg
                    )
                | exn -> Logs.err (fun m -> m "Unhandled exception: %a" Fmt.exn exn)
            );
            Lwt_io.eprintf "Listening on port %d\n%!" config.listen_port >>= fun () ->
            Cohttp_lwt_unix.Server.create
                ~mode:(`TCP (`Port config.listen_port))
                (Cohttp_lwt_unix.Server.make ~callback:(make_handler pool pubsub clients crypto config.docroot) ~conn_closed ())
        | Error _ -> Lwt.return (print_endline ("Db read/write connection failed!"))
    )
    | Error msg -> Lwt.return (print_endline ("Db pubsub connection failed: " ^ msg))

let run (config: Config.t) =
    ignore(print_endline(Info.to_string (Info.get ())));
    let random_generator = Nocrypto.Rng.create (module Nocrypto.Rng.Generators.Fortuna) in
    Nocrypto_entropy_unix.initialize ();
    Nocrypto_entropy_unix.reseed random_generator;
    Lwt_main.run (start_server config)

let () =
    match Config.parse () with
    | `Ok c -> run c
    | r -> Cmdliner.Term.exit r
