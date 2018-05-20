let type_key = "type"
let game_id_key = "game_id"
let player_id_key = "player_id"
let player_name_key = "name"
let presence_key = "presence"
let idx_key = "idx"
let card_id_key = "card_id"
let card_idx_key = "card_idx"
let score_key = "score"
let players_key = "players"
let status_key = "status"
let game_status_key = "game_status"
let card0_id_key = "card0_id"
let card1_id_key = "card1_id"
let card2_id_key = "card2_id"
let board_key = "board"
let messages_key = "messages"

type message_type =
    | Scoreboard_type
    | Player_name_type
    | Board_card_type
    | Game_card_idx_type
    | Game_status_type
    | Score_type
    | Previous_move_type
    | Player_presence_type
    | Batch_type

let message_type_to_string = function
    | Scoreboard_type -> "scoreboard"
    | Player_name_type -> "player_name"
    | Board_card_type -> "board_card"
    | Game_card_idx_type -> "game_card_idx"
    | Game_status_type -> "game_status"
    | Score_type -> "score"
    | Previous_move_type -> "previous_move"
    | Player_presence_type -> "presence"
    | Batch_type -> "batch"

let message_type_of_string = function
    | "scoreboard" -> Scoreboard_type
    | "player_name" -> Player_name_type
    | "board_card" -> Board_card_type
    | "game_card_idx" -> Game_card_idx_type
    | "game_status" -> Game_status_type
    | "score" -> Score_type
    | "previous_move" -> Previous_move_type
    | "presence" -> Player_presence_type
    | "batch" -> Batch_type
    | ts -> raise (Invalid_argument ("Unknown message type string: " ^ ts))

type game_status_data =
    | New
    | Pending
    | Started
    | Complete

let game_status_data_to_string = function
    | New -> "new"
    | Pending -> "pending"
    | Started -> "started"
    | Complete -> "complete"

let game_status_data_of_string = function
    | "new" -> New
    | "pending" -> Pending
    | "started" -> Started
    | "complete" -> Complete
    | ts -> raise (Invalid_argument ("Unknown game status: " ^ ts))

type scoreboard_player_data = {
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

type scoreboard_data = {
    players: scoreboard_player_data list;
    board: (Card.t option) array;
    game_status: game_status_data;
}

type game_card_idx_data = {
    card_idx: int;
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

type t =
    | Scoreboard of scoreboard_data
    | Player_name of player_name_data
    | Board_card of board_card_data
    | Game_card_idx of game_card_idx_data
    | Game_status of game_status_data
    | Score of score_data
    | Previous_move of previous_move_data
    | Player_presence of player_presence_data
    | Batch of t list

let make_scoreboard players board game_status =
    Scoreboard {
        players;
        board;
        game_status;
    }

let make_scoreboard_player_data player_id name presence score =
    {
        player_id;
        name;
        presence;
        score;
    }

let make_player_name player_id name =
    Player_name {
        player_id;
        name;
    }

let make_board_card_data idx card_id =
    {
        idx;
        card = Some (Card.of_int card_id);
    }

let make_board_card idx card_id =
    Board_card (make_board_card_data idx card_id)

let make_game_card_idx card_idx =
    Game_card_idx {
       card_idx;
    }

let make_game_status s =
    Game_status (game_status_data_of_string s)

let make_score player_id score =
    Score {
        player_id;
        score;
    }

let make_previous_move card0_id card1_id card2_id =
    Previous_move {
        card0 = Card.of_int card0_id;
        card1 = Card.of_int card1_id;
        card2 = Card.of_int card2_id;
    }

let make_player_presence player_id presence =
    Player_presence {
        player_id;
        presence;
    }

let make_batch msgs = Batch msgs

let scoreboard_player_data_to_string (p: scoreboard_player_data) =
    Printf.sprintf "{player_id=%d name='%s' presence=%b score=%i}"
        p.player_id p.name p.presence p.score

let card_opt_to_string = function
    | Some c -> Card.to_string c
    | None -> "[NONE]"

let board_card_data_to_string (b: board_card_data) =
    Printf.sprintf "{idx=%d card=%s}" b.idx (card_opt_to_string b.card)

let rec to_string = function
    | Scoreboard d ->
        let player_strs = List.map scoreboard_player_data_to_string d.players in
        let players = String.concat ", " player_strs in
        let board_strs = Array.map card_opt_to_string d.board |> Array.to_list in
        let board = String.concat ", " board_strs in
        let game_status = game_status_data_to_string d.game_status in
        Printf.sprintf "<message (%s) game_status=%s players=[%s] board=[%s]>"
            (message_type_to_string Scoreboard_type) game_status players board
    | Player_name d ->
        Printf.sprintf "<message (%s) player_id=%d name='%s'>"
            (message_type_to_string Player_name_type) d.player_id d.name
    | Board_card d ->
        Printf.sprintf "<message (%s) idx=%d card=%s>"
            (message_type_to_string Board_card_type) d.idx (card_opt_to_string d.card)
    | Game_card_idx d ->
        Printf.sprintf "<message (%s) card_idx=%d>"
            (message_type_to_string Game_card_idx_type) d.card_idx
    | Game_status d ->
        Printf.sprintf "<message (%s) status=%s>"
            (message_type_to_string Game_status_type) (game_status_data_to_string d)
    | Score d ->
        Printf.sprintf "<message (%s) player_id=%d score=%d>"
            (message_type_to_string Score_type) d.player_id d.score
    | Previous_move d ->
        Printf.sprintf "<message (%s) card0=%s card1=%s card2=%s>"
            (message_type_to_string Previous_move_type) (Card.to_string d.card0) (Card.to_string d.card1) (Card.to_string d.card2)
    | Player_presence d ->
        Printf.sprintf "<message (%s) player_id=%d presence=%b>"
            (message_type_to_string Player_presence_type) d.player_id d.presence
    | Batch d ->
        let msgs = List.map to_string d in
        Printf.sprintf "<message (%s) messages=[%s]>"
            (message_type_to_string Batch_type) (String.concat ", " msgs)

module type CONVERT = sig
    val to_json : t -> string
    val of_json : string -> t
end
