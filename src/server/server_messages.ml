open Shared
open Shared.Messages

module Server_message_converter : Messages.CONVERT = struct
    open Yojson.Basic

    let to_json x =
        let rec aux = function
            | Scoreboard d ->
                let players = List.map (fun (p: scoreboard_player_data) ->
                    `Assoc [
                        (player_id_key, `Int p.player_id);
                        (player_name_key, `String p.name);
                        (presence_key, `Bool p.presence);
                        (score_key, `Int p.score);
                    ]
                ) d.players
                in
                let board = List.map (fun (b: board_card_data) ->
                    `Assoc [
                        (idx_key, `Int b.idx);
                        (card_id_key, `Int b.card_id);
                    ]
                ) d.board
                in
                `Assoc [
                    (type_key, `String (message_type_to_string Scoreboard_type));
                    (players_key, `List players);
                    (board_key, `List board);
                ]
            | Player_name d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Player_name_type));
                    (player_id_key, `Int d.player_id);
                    (player_name_key, `String d.name);
                ]
            | Board_card d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Board_card_type));
                    (idx_key, `Int d.idx);
                    (card_id_key, `Int d.card_id);
                ]
            | Game_card_idx d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Game_card_idx_type));
                    (card_idx_key, `Int d.card_idx);
                ]
            | Game_status d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Game_status_type));
                    (status_key, `String (game_status_data_to_string d));
                ]
            | Score d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Score_type));
                    (player_id_key, `Int d.player_id);
                    (score_key, `Int d.score);
                ]
            | Previous_move d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Previous_move_type));
                    (card0_id_key, `Int d.card0_id);
                    (card1_id_key, `Int d.card1_id);
                    (card2_id_key, `Int d.card2_id);
                ]
            | Player_presence d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Player_presence_type));
                    (player_id_key, `Int d.player_id);
                    (player_name_key, `Bool d.presence);
                ]
            | Batch d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Batch_type));
                    (messages_key, `List (List.map aux d));
                ]
        in
        aux x |> to_string


    let of_json str =
        let rec aux json =
            let message_type = json |> Util.member type_key |> Util.to_string |> message_type_of_string in
            match message_type with
                | Scoreboard_type ->
                    let players_json = json |> Util.member players_key |> Util.to_list in
                    let players = List.map (fun json ->
                        make_scoreboard_player_data
                        (json |> Util.member player_id_key |> Util.to_int)
                        (json |> Util.member player_name_key |> Util.to_string)
                        (json |> Util.member presence_key |> Util.to_bool)
                        (json |> Util.member score_key |> Util.to_int)
                    ) players_json in

                    let board_json = json |> Util.member board_key |> Util.to_list in
                    let board = List.map (fun json ->
                        make_board_card_data
                        (json |> Util.member idx_key |> Util.to_int)
                        (json |> Util.member card_id_key |> Util.to_int)
                    ) board_json in

                    make_scoreboard players board
                | Player_name_type -> make_player_name
                    (json |> Util.member player_id_key |> Util.to_int)
                    (json |> Util.member player_name_key |> Util.to_string)
                | Board_card_type -> make_board_card
                    (json |> Util.member idx_key |> Util.to_int)
                    (json |> Util.member card_id_key |> Util.to_int)
                | Game_card_idx_type -> make_game_card_idx
                    (json |> Util.member card_idx_key |> Util.to_int)
                | Game_status_type -> make_game_status
                    (json |> Util.member status_key |> Util.to_string)
                | Score_type -> make_score
                    (json |> Util.member player_id_key |> Util.to_int)
                    (json |> Util.member score_key |> Util.to_int)
                | Previous_move_type -> make_previous_move
                    (json |> Util.member card0_id_key |> Util.to_int)
                    (json |> Util.member card1_id_key |> Util.to_int)
                    (json |> Util.member card2_id_key |> Util.to_int)
                | Player_presence_type -> make_player_presence
                    (json |> Util.member player_id_key |> Util.to_int)
                    (json |> Util.member presence_key |> Util.to_bool)
                | Batch_type ->
                    let messages = json |> Util.member messages_key |> Util.to_list in
                    make_batch @@ List.map aux messages
        in
        let json = from_string str in
        aux json
end
