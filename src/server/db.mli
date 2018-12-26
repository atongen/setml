type mode =
    | ReadUncommitted
    | ReadCommitted
    | RepeatableRead
    | Serializable

type t
val make : ?max_size:int -> string -> (t, Caqti_error.t) result Lwt.t
val create_game : ?dim0:int -> ?dim1:int -> t -> (int, Caqti_error.t) result Lwt.t
val create_game_from_previous : game_id:int -> t -> (int, Caqti_error.t) result Lwt.t
val game_exists : game_id:int -> t -> (bool, Caqti_error.t) result Lwt.t
val is_game_active : game_id:int -> t -> (bool, Caqti_error.t) result Lwt.t
val find_game_card_idx : game_id:int -> t -> (int, Caqti_error.t) result Lwt.t
val create_player : t -> (int, Caqti_error.t) result Lwt.t
val player_exists : player_id:int -> t -> (bool, Caqti_error.t) result Lwt.t
val set_game_player_presence : game_id:int -> player_id:int -> present:bool -> t -> (unit, Caqti_error.t) result Lwt.t
val find_game_player_presence : game_id:int -> player_id:int -> t -> (bool, Caqti_error.t) result Lwt.t
val is_player_member : game_id:int -> player_id:int -> t -> (bool, Caqti_error.t) result Lwt.t
val can_join : game_id:int -> player_id:int -> t -> (bool, Caqti_error.t) result Lwt.t
val create_move : game_id:int -> player_id:int -> cards:(Shared.Messages.card_data * Shared.Messages.card_data * Shared.Messages.card_data) -> t -> (int, Caqti_error.t) result Lwt.t
val create_shuffle : game_id:int -> player_id:int -> t -> (bool, Caqti_error.t) result Lwt.t
val is_game_over : game_id:int -> t -> (bool, Caqti_error.t) result Lwt.t
val start_game : game_id:int -> t -> (unit, Caqti_error.t) result Lwt.t
val end_game : game_id:int -> t -> (unit, Caqti_error.t) result Lwt.t
val update_game_theme : game_id:int -> theme:Shared.Theme.t -> t -> (unit, Caqti_error.t) result Lwt.t
val update_player_name : player_id:int -> name:string -> t -> (unit, Caqti_error.t) result Lwt.t
val get_player_name : player_id:int -> t -> (string, Caqti_error.t) result Lwt.t
val find_board_cards : game_id:int -> t -> (Shared.Messages.board_card_data list, Caqti_error.t) result Lwt.t
val find_game_cards : game_id:int -> ?offset:int -> t -> (Shared.Messages.card_data list, Caqti_error.t) result Lwt.t
val find_player_data : game_id:int -> t -> (Shared.Messages.player_data list, Caqti_error.t) result Lwt.t
val delete_all : t -> (unit, Caqti_error.t) result Lwt.t
val find_game_data : game_id:int -> t -> (Shared.Messages.game_update_data, Caqti_error.t) result Lwt.t
val find_player_games : ?limit:int -> player_id:int -> t -> (Shared.Api_messages.player_game list, Caqti_error.t) result Lwt.t
