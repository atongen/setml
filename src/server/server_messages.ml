open Shared
open Shared.Messages

module Server_message_converter : Messages.CONVERT = struct
    open Yojson.Basic

    let card_data_to_json card_data =
        `Assoc [
            (type_key, `String (message_type_to_string Server_card_type));
            (idx_key, `Int card_data.idx);
            (card_id_key, `Int (Card.to_int card_data.card));
        ]

    let to_json x =
        let rec aux = function
            | Server_game d ->
                let player_data = List.map (fun pd -> aux @@ Server_player pd) d.player_data in
                let board_card_data = List.map (fun c -> aux @@ Server_board_card c) d.board_card_data in
                let game_update_data = Server_game_update d.game_update_data in
                `Assoc [
                    (type_key, `String (message_type_to_string Server_game_type));
                    (player_data_key, `List player_data);
                    (board_card_data_key, `List board_card_data);
                    (game_update_key, aux @@ game_update_data);
                ]
            | Server_player d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Server_player_type));
                    (player_id_key, `Int d.player_id);
                    (name_key, `String d.name);
                    (presence_key, `Bool d.presence);
                    (score_key, `Int d.score);
                    (shuffles_key, `Int d.shuffles);
                ]
            | Server_name d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Server_name_type));
                    (player_id_key, `Int d.player_id);
                    (name_key, `String d.name);
                ]
            | Server_card d -> card_data_to_json d
            | Server_board_card d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Server_board_card_type));
                    (idx_key, `Int d.idx);
                    (card_id_key, `Int (Card.to_int_opt d.card));
                ]
            | Server_game_update d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Server_game_update_type));
                    (card_idx_key, `Int d.card_idx);
                    (status_key, `String (game_status_data_to_string d.status));
                ]
            | Server_score d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Server_score_type));
                    (player_id_key, `Int d.player_id);
                    (score_key, `Int d.score);
                ]
            | Server_move d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Server_move_type));
                    (card0_key, card_data_to_json d.card0);
                    (card1_key, card_data_to_json d.card1);
                    (card2_key, card_data_to_json d.card2);
                ]
            | Server_presence d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Server_presence_type));
                    (player_id_key, `Int d.player_id);
                    (presence_key, `Bool d.presence);
                ]
            | Server_move_info d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Server_move_info_type));
                    (score_data_key, aux @@ Server_score d.score_data);
                    (move_data_key, aux @@ Server_move d.move_data);
                ]
            | Server_shuffles d ->
                `Assoc [
                    (type_key, `String (message_type_to_string Server_shuffles_type));
                    (player_id_key, `Int d.player_id);
                    (shuffles_key, `Int d.shuffles);
                ]
            | Client_move (token, d) ->
                `Assoc [
                    (type_key, `String (message_type_to_string Client_move_type));
                    (token_key, `String (token_to_string token));
                    (card0_key, card_data_to_json d.card0);
                    (card1_key, card_data_to_json d.card1);
                    (card2_key, card_data_to_json d.card2);
                ]

        in
        aux x |> to_string

    let player_data_of_json json =
        make_player_data
        (json |> Util.member player_id_key |> Util.to_int)
        (json |> Util.member name_key |> Util.to_string)
        (json |> Util.member presence_key |> Util.to_bool)
        (json |> Util.member score_key |> Util.to_int)
        (json |> Util.member shuffles_key |> Util.to_int)

    let score_data_of_json json =
        make_score_data
        (json |> Util.member player_id_key |> Util.to_int)
        (json |> Util.member score_key |> Util.to_int)

    let card_data_of_json json =
        make_card_data
        (json |> Util.member idx_key |> Util.to_int)
        (json |> Util.member card_id_key |> Util.to_int)

    let board_card_data_of_json json =
        make_board_card_data
        (json |> Util.member idx_key |> Util.to_int)
        (json |> Util.member card_id_key |> Util.to_int)

    let move_data_of_json json = {
        card0 = (json |> Util.member card0_key |> card_data_of_json);
        card1 = (json |> Util.member card1_key |> card_data_of_json);
        card2 = (json |> Util.member card2_key |> card_data_of_json);
    }

    let game_update_data_of_json json =
        make_game_update_data
        (json |> Util.member card_idx_key |> Util.to_int)
        (json |> Util.member status_key |> Util.to_string)

    let name_data_of_json json =
        make_name_data
        (json |> Util.member player_id_key |> Util.to_int)
        (json |> Util.member name_key |> Util.to_string)

    let presence_data_of_json json =
        make_presence_data
        (json |> Util.member player_id_key |> Util.to_int)
        (json |> Util.member presence_key |> Util.to_bool)

    let shuffles_data_of_json json =
        make_shuffles_data
        (json |> Util.member player_id_key |> Util.to_int)
        (json |> Util.member shuffles_key |> Util.to_int)

    let of_json str =
        let aux json =
            let message_type = json |> Util.member type_key |> Util.to_string |> message_type_of_string in
            match message_type with
                | Server_game_type ->
                    let player_data = json |> Util.member player_data_key |> Util.to_list
                        |> List.map player_data_of_json in
                    let board_card_data = json |> Util.member board_card_data_key |> Util.to_list
                        |> List.map board_card_data_of_json in
                    let game_update = json |> Util.member game_update_key |> game_update_data_of_json in
                    make_server_game player_data board_card_data game_update
                | Server_player_type -> Server_player (player_data_of_json json)
                | Server_name_type -> Server_name (name_data_of_json json)
                | Server_card_type -> Server_card (card_data_of_json json)
                | Server_board_card_type -> Server_board_card (board_card_data_of_json json)
                | Server_game_update_type -> Server_game_update (game_update_data_of_json json)
                | Server_score_type -> Server_score (score_data_of_json json)
                | Server_move_type -> Server_move (move_data_of_json json)
                | Server_presence_type -> Server_presence (presence_data_of_json json)
                | Server_move_info_type ->
                    let score_data = json |> Util.member score_data_key |> score_data_of_json in
                    let move_data = json |> Util.member move_data_key |> move_data_of_json in
                    Server_move_info { score_data; move_data }
                | Server_shuffles_type -> Server_shuffles (shuffles_data_of_json json)
                | Client_move_type ->
                    let token = json |> Util.member token_key |> Util.to_string |> token_of_string in
                    let move_data = move_data_of_json json in
                    Client_move (token, move_data)
        in
        let json = from_string str in
        aux json
end
