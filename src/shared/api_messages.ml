let id_key = "id"
let status_key = "status"
let card_idx_key = "card_idx"
let joined_at_key = "joined_at"

type player_game = {
    id: int;
    status: Game_status.t;
    card_idx: int;
    joined_at: float;
}

let make_player_game id status card_idx joined_at = {
    id; status; card_idx; joined_at
}

let player_game_to_string player_game =
    Printf.sprintf "<Player_game game_id=%d status=%s card_idx=%d joined_at=%f>"
        player_game.id
        (Game_status.to_string player_game.status)
        player_game.card_idx
        player_game.joined_at


module type CONVERT = sig
    val player_games_to_json : player_game list -> string
    val player_games_of_json : string -> player_game list
end
