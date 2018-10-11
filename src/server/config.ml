open Cmdliner

type t = {
    setml_env: string;
    db_user: string;
    db_name: string;
    port: int;
}

let setml_env () =
    match Sys.getenv_opt "SETML_ENV" with
    | Some e -> e
    | None -> "development"

let default_user () =
    let whoami = CCUnix.with_process_in "whoami" ~f:CCIO.read_line in
    match whoami with
    | Some u -> u
    | None -> "setml"

let db_user =
    let doc = "Database user name" in
    let env = Arg.env_var "DB_USER" ~doc in
    Arg.(value & opt string (default_user ()) & info ["db-user"] ~env ~docv:"DB_USER" ~doc)

let db_name =
    let doc = "Database name" in
    let env = Arg.env_var "DB_NAME" ~doc in
    let default = "setml_" ^ (setml_env ()) in
    Arg.(value & opt string default & info ["db-name"] ~env ~docv:"DB_NAME" ~doc)

let port =
    let doc = "Listen port" in
    let env = Arg.env_var "PORT" ~doc in
    Arg.(value & opt int 7777 & info ["p"; "port"] ~env ~docv:"PORT" ~doc)

let info =
    let doc = "setml" in
    let man = [
        `S Manpage.s_bugs;
        `P "Email bug reports to <hehey at example.org>." ]
    in
    Term.info "setml" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

let make db_user db_name port =
    {
        setml_env = setml_env ();
        db_user;
        db_name;
        port;
    }

let config_t = Term.(const make $ db_user $ db_name $ port)

let parse () = Term.eval (config_t, info)
