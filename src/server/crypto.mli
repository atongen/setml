type t
val make : ?salt:string -> string -> t
val create_iv : unit -> Cstruct.t
val verify_and_decrypt : t -> string -> ((Cstruct.t * string), string) result
val encrypt_and_sign_with_iv : t -> Cstruct.t -> string -> string
val encrypt_and_sign : t -> string -> string