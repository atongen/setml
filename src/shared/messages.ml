let type_key = "type"
let game_id_key = "game_id"
let player_id_key = "player_id"
let name_key = "name"
let presence_key = "presence"
let idx_key = "idx"
let card_id_key = "card_id"
let card_idx_key = "card_idx"
let score_key = "score"
let previous_move_key = "previous_move"
let player_data_key = "player_data"
let status_key = "status"
let card0_id_key = "card0_id"
let card1_id_key = "card1_id"
let card2_id_key = "card2_id"
let board_data_key = "board_data"
let game_update_key = "game_update"

type message_type =
    | Game_data_type
    | Player_data_type
    | Player_name_type
    | Board_card_type
    | Game_update_type
    | Score_type
    | Previous_move_type
    | Player_presence_type
    | Move_data_type

let message_type_to_string = function
    | Game_data_type -> "game_data"
    | Player_data_type -> "player_data"
    | Player_name_type -> "player_name"
    | Board_card_type -> "board_card"
    | Game_update_type -> "game_update"
    | Score_type -> "score"
    | Previous_move_type -> "previous_move"
    | Player_presence_type -> "presence"
    | Move_data_type -> "move_data"

let message_type_of_string = function
    | "game_data" -> Game_data_type
    | "player_data" -> Player_data_type
    | "player_name" -> Player_name_type
    | "board_card" -> Board_card_type
    | "game_update" -> Game_update_type
    | "score" -> Score_type
    | "previous_move" -> Previous_move_type
    | "presence" -> Player_presence_type
    | "move_data" -> Move_data_type
    | ts -> raise (Invalid_argument ("Unknown message type string: " ^ ts))

type game_status_data =
    | New
    | Started
    | Complete

let game_status_data_to_string = function
    | New -> "new"
    | Started -> "started"
    | Complete -> "complete"

let game_status_data_of_string = function
    | "new" -> New
    | "started" -> Started
    | "complete" -> Complete
    | ts -> raise (Invalid_argument ("Unknown game status: " ^ ts))

type player_data = {
    player_id: int;
    name: string;
    presence: bool;
    score: int;
}

type player_name_data = {
    player_id: int;
    name: string;
}

type board_card_data = {
    idx: int;
    card: Card.t option;
}

type game_update_data = {
    card_idx: int;
    status: game_status_data;
}

type game_data = {
    player_data: player_data list;
    board_data: (Card.t option) array;
    game_update: game_update_data;
}

type score_data = {
    player_id: int;
    score: int;
}

type previous_move_data = {
    card0: Card.t;
    card1: Card.t;
    card2: Card.t;
}

type player_presence_data = {
    player_id: int;
    presence: bool;
}

type move_data = {
    score: score_data;
    previous_move: previous_move_data;
}

type t =
    | Game_data of game_data
    | Player_data of player_data
    | Player_name of player_name_data
    | Board_card of board_card_data
    | Game_update of game_update_data
    | Score of score_data
    | Previous_move of previous_move_data
    | Player_presence of player_presence_data
    | Move_data of move_data

let make_game_data player_data board_data game_update =
    Game_data {
        player_data;
        board_data;
        game_update;
    }

let make_player_data player_id name presence score =
    {
        player_id;
        name;
        presence;
        score;
    }

let make_player_name_data player_id name =
    {
        player_id;
        name;
    }

let make_player_name player_id name =
    Player_name (make_player_name_data player_id name)

let make_board_card_data idx card_id =
    {
        idx;
        card = Card.of_int_opt card_id;
    }

let make_board_card idx card_id =
    Board_card (make_board_card_data idx card_id)

let make_board_cards bc_arr =
    Array.mapi (fun idx card ->
        Board_card {idx; card}
    ) bc_arr

let make_game_update_data status card_idx =
    {
       status = (game_status_data_of_string status);
       card_idx;
    }

let make_game_update status card_idx =
    Game_update (make_game_update_data status card_idx)

let make_score_data player_id score =
    {
        player_id;
        score;
    }

let make_score player_id score =
    Score (make_score_data player_id score)

let make_previous_move_data card0_id card1_id card2_id =
    {
        card0 = Card.of_int card0_id;
        card1 = Card.of_int card1_id;
        card2 = Card.of_int card2_id;
    }

let make_previous_move card0_id card1_id card2_id =
    Previous_move (make_previous_move_data card0_id card1_id card2_id)

let make_player_presence_data player_id presence =
    {
        player_id;
        presence;
    }

let make_player_presence player_id presence =
    Player_presence (make_player_presence_data player_id presence)

let make_move_data score previous_move =
    Move_data {
        score;
        previous_move;
    }

let card_opt_to_string = function
    | Some c -> Card.to_string c
    | None -> "{NONE}"

let rec to_string = function
    | Game_data d ->
        let player_strs = List.map (fun pd -> Player_data pd |> to_string) d.player_data in
        let players = String.concat ", " player_strs in
        let board_strs = make_board_cards d.board_data
            |> Array.to_list
            |> List.map to_string
        in
        let board = String.concat ", " board_strs in
        let game_update = to_string @@ Game_update d.game_update in
        Printf.sprintf "<message (%s) game_update=%s players=[%s] board=[%s]>"
            (message_type_to_string Game_data_type) game_update players board
    | Player_data d ->
        Printf.sprintf "<message (%s) id=%d name='%s' presence=%b score=%i>"
            (message_type_to_string Player_data_type) d.player_id d.name d.presence d.score
    | Player_name d ->
        Printf.sprintf "<message (%s) player_id=%d name='%s'>"
            (message_type_to_string Player_name_type) d.player_id d.name
    | Board_card d ->
        Printf.sprintf "<message (%s) idx=%d card=%s>"
            (message_type_to_string Board_card_type) d.idx (card_opt_to_string d.card)
    | Game_update d ->
        Printf.sprintf "<message (%s) card_idx=%d status=%s>"
            (message_type_to_string Game_update_type) d.card_idx (game_status_data_to_string d.status)
    | Score d ->
        Printf.sprintf "<message (%s) player_id=%d score=%d>"
            (message_type_to_string Score_type) d.player_id d.score
    | Previous_move d ->
        Printf.sprintf "<message (%s) card0=%s card1=%s card2=%s>"
            (message_type_to_string Previous_move_type) (Card.to_string d.card0) (Card.to_string d.card1) (Card.to_string d.card2)
    | Player_presence d ->
        Printf.sprintf "<message (%s) player_id=%d presence=%b>"
            (message_type_to_string Player_presence_type) d.player_id d.presence
    | Move_data d ->
        let score = to_string @@ Score d.score in
        let previous_move = to_string @@ Previous_move d.previous_move in
        Printf.sprintf "<message (%s) score=%s previous_move=%s>"
            (message_type_to_string Move_data_type) score previous_move

module type CONVERT = sig
    val to_json : t -> string
    val of_json : string -> t
end
