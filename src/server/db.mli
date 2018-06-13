type t
val create : ?max_size:int -> Uri.t -> (t, string) result Lwt.t
val create_game : t -> unit -> (int, string) result Lwt.t
val game_exists : t -> int -> (bool, string) result Lwt.t
val create_player : t -> unit -> (int, string) result Lwt.t
val player_exists : t -> int -> (bool, string) result Lwt.t
val game_player_presence : t -> (int * int * bool) -> (unit, string) result Lwt.t
val increment_game_card_idx : t -> (int * int) -> (int, string) result Lwt.t
val create_move : t -> (int * int * int * Shared.Card.t * int * Shared.Card.t * int * Shared.Card.t) -> (bool, string) result Lwt.t

val delete_all : t -> unit -> (unit, string) result Lwt.t
