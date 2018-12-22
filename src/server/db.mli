type mode =
    | ReadUncommitted
    | ReadCommitted
    | RepeatableRead
    | Serializable

type t
val make : ?max_size:int -> string -> (t, Caqti_error.t) result Lwt.t
val create_game : t -> ?dim0:int -> ?dim1:int -> unit -> (int, Caqti_error.t) result Lwt.t
val create_game_from_previous : t -> int -> (int, Caqti_error.t) result Lwt.t
val game_exists : t -> int -> (bool, Caqti_error.t) result Lwt.t
val find_game_card_idx : t -> int -> (int, Caqti_error.t) result Lwt.t
val create_player : t -> unit -> (int, Caqti_error.t) result Lwt.t
val player_exists : t -> int -> (bool, Caqti_error.t) result Lwt.t
val set_game_player_presence : t -> (int * int * bool) -> (unit, Caqti_error.t) result Lwt.t
val find_game_player_presence : t -> (int * int) -> (bool, Caqti_error.t) result Lwt.t
val increment_game_card_idx : t -> (int * int) -> (int, Caqti_error.t) result Lwt.t
val create_move : t -> (int * int * (Shared.Messages.card_data * Shared.Messages.card_data * Shared.Messages.card_data)) -> (int, Caqti_error.t) result Lwt.t
val create_shuffle : t -> (int * int) -> (bool, Caqti_error.t) result Lwt.t
val is_game_over : t -> int -> (bool, Caqti_error.t) result Lwt.t
val start_game : t -> int -> (unit, Caqti_error.t) result Lwt.t
val end_game : t -> int -> (unit, Caqti_error.t) result Lwt.t
val update_game_theme : t -> (int * Shared.Theme.t) -> (unit, Caqti_error.t) result Lwt.t
val update_player_name : t -> (int * string) -> (unit, Caqti_error.t) result Lwt.t
val get_player_name : t -> int -> (string, Caqti_error.t) result Lwt.t
val find_board_cards : t -> int -> (Shared.Messages.board_card_data list, Caqti_error.t) result Lwt.t
val find_game_cards : t -> (int * int) -> (Shared.Messages.card_data list, Caqti_error.t) result Lwt.t
val find_player_data : t -> int -> (Shared.Messages.player_data list, Caqti_error.t) result Lwt.t
val delete_all : t -> unit -> (unit, Caqti_error.t) result Lwt.t
val find_game_data : t -> int -> (Shared.Messages.game_update_data, Caqti_error.t) result Lwt.t
val find_player_games : t -> ?limit:int -> int -> (Shared.Api_messages.player_game list, Caqti_error.t) result Lwt.t
