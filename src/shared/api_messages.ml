let id_key = "id"
let status_key = "status"
let card_idx_key = "card_idx"
let updated_at_key = "updated_at"

type player_game = {
    id: int;
    status: Game_status.t;
    card_idx: int;
    updated_at: float;
}

let make_player_game id status card_idx updated_at = {
    id; status; card_idx; updated_at
}

let player_game_to_string player_game =
    Printf.sprintf "<Player_game game_id=%d status=%s card_idx=%d updated_at=%f>"
        player_game.id
        (Game_status.to_string player_game.status)
        player_game.card_idx
        player_game.updated_at


module type CONVERT = sig
    val player_games_to_json : player_game list -> string
    val player_games_of_json : string -> player_game list
end
