type t

val make : ?n:int -> ?retries:int -> string -> Clients.t -> (t, string) result
val subscribe : t -> int -> unit
val unsubscribe : t -> int -> unit
val get_notifications : t -> Postgresql.Notification.t list
val empty_query : t -> string -> unit
val start : t -> unit
