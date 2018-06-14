type t
val create_game : t -> unit -> (int, string) result Lwt.t
val game_exists : t -> int -> (bool, string) result Lwt.t
val create_player : t -> unit -> (int, string) result Lwt.t
val player_exists : t -> int -> (bool, string) result Lwt.t
val game_player_presence : t -> (int * int * bool) -> (unit, string) result Lwt.t
val increment_game_card_idx : t -> (int * int) -> (int, string) result Lwt.t
val create_move : t -> (int * int * int * Shared.Card.t * int * Shared.Card.t * int * Shared.Card.t) -> (int, string) result Lwt.t
val shuffle_board : t -> (int * int) -> (bool, string) result Lwt.t
val is_game_over : t -> int -> (bool, string) result Lwt.t
val update_player_name : t -> (int * string) -> (unit, string) result Lwt.t
val find_board_cards : t -> int -> (int list, string) result Lwt.t
val find_game_cards : t -> (int * int) -> ((int * int) list, string) result Lwt.t
val find_player_data : t -> int -> (Shared.Messages.player_data list, string) result Lwt.t
val delete_all : t -> unit -> (unit, string) result Lwt.t
