open Shared
open Shared.Messages

module Server_message_converter : Messages.CONVERT = struct
    open Yojson.Basic

    let to_json = function
        | Presence (pt) ->
            `Assoc [
                (type_key, `String (message_type_to_string Presence_type));
                (game_id_key, `Int pt.game_id);
                (player_id_key, `Int pt.player_id);
                (player_name_key, `String pt.player_name);
                (value_key, `Bool pt.value);
            ]
            |> to_string

    let of_json str =
        let json = from_string str in
        let message_type = json |> Util.member type_key |> Util.to_string |> message_type_of_string in
        match message_type with
            | Presence_type -> make_presence
                (json |> Util.member game_id_key |> Util.to_int)
                (json |> Util.member player_id_key |> Util.to_int)
                (json |> Util.member player_name_key |> Util.to_string)
                (json |> Util.member value_key |> Util.to_bool)
end
