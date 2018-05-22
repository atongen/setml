open Shared
open Shared.Messages

module Server_message_converter : Messages.CONVERT = struct
    open Yojson.Basic

    let to_json x =
        let rec aux = function
            | Game_data d ->
                let player_data = List.map (fun pd -> aux @@ Player_data pd ) d.player_data in
                let board_data = make_board_cards d.board_data
                    |> Array.to_list
                    |> List.map aux
                in
                `Assoc [
                    (type_key, `String (message_type_to_string Game_data_type));
                    (player_data_key, `List player_data);
                    (board_data_key, `List board_data);
                    (game_update_key, aux @@ Game_update d.game_update);
                ]
            | Player_data d ->
                `Assoc [
                    (player_id_key, `Int d.player_id);
                    (name_key, `String d.name);
                    (presence_key, `Bool d.presence);
                    (score_key, `Int d.score);
                ]
            | Player_name d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Player_name_type));
                    (player_id_key, `Int d.player_id);
                    (name_key, `String d.name);
                ]
            | Board_card d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Board_card_type));
                    (idx_key, `Int d.idx);
                    (card_id_key, `Int (Card.to_int_opt d.card));
                ]
            | Game_update d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Game_update_type));
                    (card_idx_key, `Int d.card_idx);
                    (status_key, `String (game_status_data_to_string d.status));
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
                    (card0_id_key, `Int (Card.to_int d.card0));
                    (card1_id_key, `Int (Card.to_int d.card1));
                    (card2_id_key, `Int (Card.to_int d.card2));
                ]
            | Player_presence d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Player_presence_type));
                    (player_id_key, `Int d.player_id);
                    (presence_key, `Bool d.presence);
                ]
            | Move_data d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Move_data_type));
                    (score_key, aux @@ Score d.score);
                    (previous_move_key, aux @@ Previous_move d.previous_move);
                ]
        in
        aux x |> to_string

    let player_data_of_json json =
        make_player_data
        (json |> Util.member player_id_key |> Util.to_int)
        (json |> Util.member name_key |> Util.to_string)
        (json |> Util.member presence_key |> Util.to_bool)
        (json |> Util.member score_key |> Util.to_int)

    let score_data_of_json json =
        make_score_data
        (json |> Util.member player_id_key |> Util.to_int)
        (json |> Util.member score_key |> Util.to_int)

    let previous_move_data_of_json json =
        make_previous_move_data
        (json |> Util.member card0_id_key |> Util.to_int)
        (json |> Util.member card1_id_key |> Util.to_int)
        (json |> Util.member card2_id_key |> Util.to_int)

    let board_card_data_of_json json =
        make_board_card_data
        (json |> Util.member idx_key |> Util.to_int)
        (json |> Util.member card_id_key |> Util.to_int)

    let game_update_data_of_json json =
        make_game_update_data
        (json |> Util.member status_key |> Util.to_string)
        (json |> Util.member card_idx_key |> Util.to_int)

    let player_name_data_of_json json =
        make_player_name_data
        (json |> Util.member player_id_key |> Util.to_int)
        (json |> Util.member name_key |> Util.to_string)

    let player_presence_data_of_json json =
        make_player_presence_data
        (json |> Util.member player_id_key |> Util.to_int)
        (json |> Util.member presence_key |> Util.to_bool)

    let of_json str =
        let rec aux json =
            let message_type = json |> Util.member type_key |> Util.to_string |> message_type_of_string in
            match message_type with
                | Game_data_type ->
                    let player_data = json |> Util.member player_data_key |> Util.to_list
                        |> List.map player_data_of_json in
                    let board_data = json |> Util.member board_data_key |> Util.to_list
                        |> List.map (fun json -> json |> Util.member card_id_key |> Util.to_int |> Card.of_int_opt)
                        |> Array.of_list in
                    let game_update = json |> Util.member game_update_key |> game_update_data_of_json in
                    make_game_data player_data board_data game_update
                | Player_data_type -> Player_data (player_data_of_json json)
                | Player_name_type -> Player_name (player_name_data_of_json json)
                | Board_card_type -> Board_card (board_card_data_of_json json)
                | Game_update_type -> Game_update (game_update_data_of_json json)
                | Score_type -> Score (score_data_of_json json)
                | Previous_move_type -> Previous_move (previous_move_data_of_json json)
                | Player_presence_type -> Player_presence (player_presence_data_of_json json)
                | Move_data_type ->
                    let score = json |> Util.member score_key |> score_data_of_json in
                    let previous_move = json |> Util.member previous_move_key |> previous_move_data_of_json in
                    make_move_data score previous_move
        in
        let json = from_string str in
        aux json
end
