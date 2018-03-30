type t

val make : ?n:int -> string -> Clients.t -> t
val subscribe : t -> string -> unit
val unsubscribe : t -> string -> unit
val start : t -> unit