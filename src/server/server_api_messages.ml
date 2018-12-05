open Shared
open Shared.Api_messages

module Server_api_message_converter : CONVERT = struct
    open Yojson.Basic

    let player_games_to_json player_games =
        `List (List.map (fun player_game ->
            `Assoc [
                (id_key, `Int player_game.id);
                (status_key, `String (Game_status.to_string player_game.status));
                (card_idx_key, `Int player_game.card_idx);
                (updated_at_key, `Float player_game.updated_at);
            ]
        ) player_games) |> to_string

    let player_game_of_json json =
        make_player_game
        (json |> Util.member id_key |> Util.to_int)
        (json |> Util.member status_key |> Util.to_string |> Game_status.of_string)
        (json |> Util.member card_idx_key |> Util.to_int)
        (json |> Util.member updated_at_key |> Util.to_float)

    let player_games_of_json str =
        str |> from_string
            |> Util.to_list
            |> List.map player_game_of_json
end
