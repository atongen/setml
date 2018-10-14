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

val parse : unit ->  t Cmdliner.Term.result
val make_of_env : unit -> t
val to_string : t -> string
val db_conninfo : t -> string
val db_uri_str : t -> string
