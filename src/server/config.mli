type t = {
    setml_env: string;
    listen_address: string;
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
