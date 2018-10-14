open Cmdliner

type t = {
    setml_env: string;
    listen_port: int;
    db_name: string;
    db_host: string;
    db_port: int;
    db_user: string;
    db_pass: string;
    db_pool: int;
    crypto_secret: string;
}

exception RequiredMissing of string

let to_string c =
    Printf.sprintf
    "{setml_env: '%s', listen_port: %d, db_name: '%s', db_host: '%s', db_port: %d, db_user: '%s', db_pass: '%s', db_pool: %d, crypto_secret: '%s'}"
    c.setml_env c.listen_port
    c.db_name c.db_host c.db_port c.db_user c.db_pass c.db_pool
    c.crypto_secret

let env_or key default =
    match Sys.getenv_opt key with
    | Some e -> e
    | None -> default

let env_or_int key default =
    match Sys.getenv_opt key with
    | Some e -> int_of_string e
    | None -> default

let env_unless_production env key default =
    match Sys.getenv_opt key with
    | Some e -> e
    | None -> if env = "production" then (
        raise (RequiredMissing key)
    ) else (
        default
    )

let setml_env_key = "SETML_ENV"
let setml_env_default = "development"

let setml_env () = env_or setml_env_key setml_env_default

let whoami () =
    let whoami = CCUnix.with_process_in "whoami" ~f:CCIO.read_line in
    match whoami with
    | Some u -> u
    | None -> "setml"

let listen_port_key = "LISTEN_PORT"
let listen_port_default = 7777

let db_name_key = "DB_NAME"
let db_name_default = "setml_" ^ (setml_env ())

let db_host_key = "DB_HOST"
let db_host_default = "localhost"

let db_port_key = "DB_PORT"
let db_port_default = 5432

let db_user_key = "DB_USER"
let db_user_default = whoami ()

let db_pass_key = "DB_PASS"
let db_pass_default = "abcd1234"

let db_pool_key = "DB_POOL"
let db_pool_default = 8

let crypto_secret_key = "CRYPTO_SECRET"
let crypto_secret_default = "t8sK8LqFLn6Ixt9H6TMiS9HRs6BfcLyw6aXHi02omeOIp7mLYqSIlxtPgxXiETvpentbHMPkGYpiqW8nR9rJmeVU4aEEyzMbzDqIRznNSiqPnDb0Dp9PNerGuODpaeza"

let setml_env_a =
    let doc = "setml environment" in
    let env = Arg.env_var setml_env_key ~doc in
    Arg.(value & pos 0 string (setml_env ()) & info [] ~env)

let listen_port =
    let doc = "Listen port" in
    let env = Arg.env_var listen_port_key ~doc in
    Arg.(value & opt int listen_port_default & info ["p"; "listen-port"] ~env ~docv:listen_port_key ~doc)

let db_name =
    let doc = "Database name" in
    let env = Arg.env_var db_name_key ~doc in
    Arg.(value & opt string db_name_default & info ["n"; "db-name"] ~env ~docv:db_name_key ~doc)

let db_host =
    let doc = "Database host" in
    let env = Arg.env_var db_host_key ~doc in
    Arg.(value & opt string db_host_default & info ["h"; "db-host"] ~env ~docv:db_host_default ~doc)

let db_port =
    let doc = "Database port" in
    let env = Arg.env_var db_port_key ~doc in
    Arg.(value & opt int db_port_default & info ["db-port"] ~env ~docv:db_port_key ~doc)

let db_user =
    let doc = "Database user name" in
    let env = Arg.env_var db_user_key ~doc in
    Arg.(value & opt string db_user_default & info ["u"; "db-user"] ~env ~docv:db_user_key ~doc)

let db_pass =
    let doc = "Database password" in
    let env = Arg.env_var db_pass_key ~doc in
    let open Arg in
    let i = info ["db-pass"] ~env ~docv:db_pass_key ~doc in
    match setml_env () with
    | "production" -> required & opt (some string) None & i
    | _ -> value & opt string db_pass_default & i

let db_pool =
    let doc = "Database connection pool size" in
    let env = Arg.env_var db_pool_key ~doc in
    Arg.(value & opt int db_pool_default & info ["db-pool"] ~env ~docv:db_pool_key ~doc)

let crypto_secret =
    let doc = "Crypto secret key" in
    let env = Arg.env_var crypto_secret_key ~doc in
    let open Arg in
    let i = info ["crypto-secret"] ~env ~docv:crypto_secret_key ~doc in
    match setml_env () with
    | "production" -> required & opt (some string) None & i
    | _ -> value & opt string crypto_secret_default & i

let info =
    let doc = "setml" in
    let i = Info.get () in
    let build_info = Info.to_string i in
    let man = [
        `S Manpage.s_bugs;
        `P i.bug_reports
    ] in
    Term.info "setml" ~version:build_info ~doc ~exits:Term.default_exits ~man

let make setml_env listen_port db_name db_host db_port db_user db_pass db_pool crypto_secret =
    {
        setml_env;
        listen_port;
        db_name;
        db_host;
        db_port;
        db_user;
        db_pass;
        db_pool;
        crypto_secret;
    }

let make_of_env () =
    let env = setml_env () in
    {
        setml_env = env;
        listen_port = env_or_int listen_port_key listen_port_default;
        db_name = env_or db_name_key db_name_default;
        db_host = env_or db_host_key db_host_default;
        db_port = env_or_int db_port_key db_port_default;
        db_user = env_or db_user_key db_user_default;
        db_pass = env_unless_production env db_pass_key db_pass_default;
        db_pool = env_or_int db_pool_key db_pool_default;
        crypto_secret = env_unless_production env crypto_secret_key crypto_secret_default;
    }

let config_t =
    let open Term in
    const make $
    setml_env_a $
    listen_port $
    db_name $ db_host $ db_port $ db_user $ db_pass $ db_pool $
    crypto_secret

let parse () = Term.eval (config_t, info)

let db_conninfo c =
    Printf.sprintf "user=%s password=%s port=%d host=%s dbname=%s"
    c.db_user c.db_pass c.db_port c.db_host c.db_name

let db_uri_str c =
    Printf.sprintf "postgresql://%s:%s@%s:%d/%s"
    c.db_user c.db_pass c.db_host c.db_port c.db_name
