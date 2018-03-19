module GameKey = CCString
module PlayerKey = CCInt
module ConnKey :
  sig
    type t
    val make : GameKey.t -> PlayerKey.t -> t
    val hash : t -> int
    val equal : t -> t -> bool
    val to_string : t -> string
  end
type t
val make : ?n:int -> unit -> t
val send : (Websocket_cohttp_lwt.Frame.t option -> unit) -> string -> unit
val send_key : t -> ConnKey.t -> string -> unit
val broadcast_send : t -> string -> unit
val game_send : t -> GameKey.t -> string -> unit
val player_send : t -> PlayerKey.t -> string -> unit
val games_of_player_send : t -> PlayerKey.t -> string -> unit
val in_game : t -> GameKey.t -> PlayerKey.t -> bool
val add : t -> GameKey.t -> PlayerKey.t -> (Websocket_cohttp_lwt.Frame.t option -> unit) -> unit
val remove : t -> GameKey.t -> PlayerKey.t -> unit
val add_or_replace : t -> GameKey.t -> PlayerKey.t -> (Websocket_cohttp_lwt.Frame.t option -> unit) -> unit