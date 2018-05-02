type t

val make : ?n:int -> ?delay:float -> string -> Clients.t -> t
val subscribe : t -> int -> unit
val unsubscribe : t -> int -> unit
val get_notifications : t -> (t -> string -> int -> string -> unit) -> int
val start : t -> unit
