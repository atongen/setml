open Shared_util


let type_key = "type"
let game_id_key = "game_id"
let player_id_key = "player_id"
let player_name_key = "player_name"
let value_key = "value"

type message_type =
    | Presence_type
    | Player_name_type

let message_type_to_string = function
    | Presence_type -> "presence"
    | Player_name_type -> "player_name"

let message_type_of_string = function
    | "presence" -> Presence_type
    | "player_name" -> Player_name_type
    | ts -> raise (Invalid_argument ("Unknown message type string: " ^ ts))

type presence_data = {
    game_id: int;
    player_id: int;
    player_name: string;
    value: bool;
}

type player_name_data = {
    game_id: int;
    player_id: int;
    player_name: string;
}

type t =
    | Presence of presence_data
    | Player_name of player_name_data

let make_presence game_id player_id player_name value =
    Presence {
        game_id;
        player_id;
        player_name;
        value;
    }

let make_player_name game_id player_id player_name =
    Player_name {
        game_id;
        player_id;
        player_name;
    }

let to_string = function
    | Presence d ->
        Printf.sprintf "<message (%s) game_id=%d player_id=%d player_name='%s' value=%b>"
            (message_type_to_string Presence_type) d.game_id d.player_id d.player_name d.value
    | Player_name d ->
        Printf.sprintf "<message (%s) game_id=%d player_id=%d player_name='%s'>"
            (message_type_to_string Player_name_type) d.game_id d.player_id d.player_name


module type CONVERT = sig
    val to_json : t -> string
    val of_json : string -> t
end
