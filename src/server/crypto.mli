type t
val init : unit -> unit
val make : ?salt:string -> string -> t
val create_iv : unit -> Cstruct.t
val random_hex : unit -> string
val random_string : int -> string
val random_int : int -> int -> int
val verify_and_decrypt : t -> string -> ((Cstruct.t * string), string) result
val encrypt_and_sign_with_iv : t -> Cstruct.t -> string -> string
val encrypt_and_sign : t -> string -> string
