module type S = sig
    type t
    val query_int : t -> string -> (int, Caqti_error.call_or_retrieve) result Lwt.t
end

module B: (S with type t := (module Caqti_lwt.CONNECTION)) = struct
    type mode =
        | ReadUncommitted
        | ReadCommitted
        | RepeatableRead
        | Serializable

    let string_of_mode = function
        | ReadUncommitted -> "READ UNCOMMITTED"
        | ReadCommitted -> "READ COMMITTED"
        | RepeatableRead -> "REPEATABLE READ"
        | Serializable -> "SERIALIZABLE"

    let with_transaction ?(mode=ReadCommitted) (module C : Caqti_lwt.CONNECTION) f arg =
        C.start () >>=? fun () ->
        C.exec (Q.set_transaction_mode_query (string_of_mode mode)) () >>=? fun () ->
        f (module C : Caqti_lwt.CONNECTION) arg >>=? fun result ->
        C.commit () >>=? fun () ->
        Lwt.return_ok result


    let query_int c q = Db.query_int c q
end

module K: (S with type t := (((module Caqti_lwt.CONNECTION), Caqti_error.call_or_retrieve) Caqti_lwt.Pool.t)) = struct
    let with_pool pool f arg =
        Caqti_lwt.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
            f (module C : Caqti_lwt.CONNECTION) arg
        ) pool

    let query_int p q = with_pool p B.query_int q
end
