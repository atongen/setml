open Cmdliner

let default_user () =
    let whoami = CCUnix.with_process_in "whoami" ~f:CCIO.read_line in
    match whoami with
    | Some u -> u
    | None -> "setml"

let setml_env =
    let doc = "SetML environment" in
    let env = Arg.env_var "SETML_ENV" ~doc in
    Arg.(value & opt string "development" & info ["setml-env"] ~env ~docv:"SETML_ENV" ~doc)

let db_user =
    let doc = "Database user name" in
    let env = Arg.env_var "DB_USER" ~doc in
    Arg.(value & opt string (default_user ()) & info ["db-user"] ~env ~docv:"DB_USER" ~doc)

let db_name =
    let doc = "Database user name" in
    let env = Arg.env_var "DB_USER" ~doc in
    Arg.(value & opt string (default_user ()) & info ["db-user"] ~env ~docv:"DB_USER" ~doc)

let info =
    let doc = "setml" in
    let man = [
        `S Manpage.s_bugs;
        `P "Email bug reports to <hehey at example.org>." ]
    in
    Term.info "setml" ~version:"%‌%VERSION%%" ~doc ~exits:Term.default_exits ~man


let thing () = Term.eval (db_user, info)
