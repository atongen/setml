module GameKey = CCInt
module PlayerKey = CCInt
type t
val make : ?n:int -> unit -> t
val send : (Websocket_cohttp_lwt.Frame.t option -> unit) -> string -> unit
val game_send : t -> GameKey.t -> string -> unit
val in_game : t -> GameKey.t -> PlayerKey.t -> bool
val game_has_players : t -> GameKey.t -> bool
val add : t -> GameKey.t -> PlayerKey.t -> (Websocket_cohttp_lwt.Frame.t option -> unit) -> unit
val remove : t -> GameKey.t -> PlayerKey.t -> unit
