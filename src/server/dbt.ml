module type S = sig
    type t
    val query_int : t -> string -> (int, Caqti_error.call_or_retrieve) result Lwt.t
end

module B: (S with type t := (module Caqti_lwt.CONNECTION)) = struct
    let query_int c q = Db.query_int c q
end

module K: (S with type t := (((module Caqti_lwt.CONNECTION), Caqti_error.call_or_retrieve) Caqti_lwt.Pool.t)) = struct
    let with_pool pool f arg =
        Caqti_lwt.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
            f (module C : Caqti_lwt.CONNECTION) arg
        ) pool

    let query_int p q = with_pool p B.query_int q
end
