type t
type err
val create : ?max_size:int -> string -> (t, err) result
val create_game : t -> unit -> (int, err) result Lwt.t
val game_exists : t -> int -> (bool, err) result Lwt.t
val create_player : t -> unit -> (int, err) result Lwt.t
val player_exists : t -> int -> (bool, err) result Lwt.t
val game_player_presence : t -> (int * int * bool) -> (unit, err) result Lwt.t
val increment_game_card_idx : t -> (int * int) -> (int, err) result Lwt.t
val create_move : t -> (int * int * int * Shared.Card.t * int * Shared.Card.t * int * Shared.Card.t) -> (int, err) result Lwt.t
val shuffle_board : t -> (int * int) -> (bool, err) result Lwt.t
val is_game_over : t -> int -> (bool, err) result Lwt.t
val update_player_name : t -> (int * string) -> (unit, err) result Lwt.t
val find_board_cards : t -> int -> (int list, err) result Lwt.t
val find_game_cards : t -> (int * int) -> ((int * int) list, err) result Lwt.t
val find_player_data : t -> int -> (Shared.Messages.player_data list, err) result Lwt.t
val delete_all : t -> unit -> (unit, err) result Lwt.t
