let type_key = "type"
let token_key = "token"
let game_id_key = "game_id"
let player_id_key = "player_id"
let name_key = "name"
let name_data_key = "name_data"
let presence_key = "presence"
let presence_data_key = "presence_data"
let idx_key = "idx"
let card_id_key = "card_id"
let card_idx_key = "card_idx"
let score_key = "score"
let score_data_key = "score_data"
let move_data_key = "move_data"
let player_data_key = "player_data"
let status_key = "status"
let card0_key = "card0"
let card1_key = "card1"
let card2_key = "card2"
let card_data_key = "card_data"
let board_card_data_key = "board_card_data"
let game_update_key = "game_update"
let shuffles_key = "shuffles"
let theme_key = "theme"
let dim0_key = "dim0"
let dim1_key = "dim1"

type message_type =
    | Server_game_type
    | Server_player_type
    | Server_name_type
    | Server_card_type
    | Server_board_card_type
    | Server_game_update_type
    | Server_score_type
    | Server_move_type
    | Server_presence_type
    | Server_move_info_type
    | Server_shuffles_type
    | Client_move_type
    | Client_shuffle_type
    | Client_start_game_type

let message_type_to_string = function
    | Server_game_type -> "server_game"
    | Server_player_type -> "server_player"
    | Server_name_type -> "server_name"
    | Server_card_type -> "server_card"
    | Server_board_card_type -> "server_board_card"
    | Server_game_update_type -> "server_game_update"
    | Server_score_type -> "server_score"
    | Server_move_type -> "server_move"
    | Server_presence_type -> "server_presence"
    | Server_move_info_type -> "server_move_info"
    | Server_shuffles_type -> "server_shuffles"
    | Client_move_type -> "client_move"
    | Client_shuffle_type -> "client_shuffle"
    | Client_start_game_type -> "client_start_game"

let message_type_of_string = function
    | "server_game" -> Server_game_type
    | "server_player" -> Server_player_type
    | "server_name" -> Server_name_type
    | "server_card" -> Server_card_type
    | "server_board_card" -> Server_board_card_type
    | "server_game_update" -> Server_game_update_type
    | "server_score" -> Server_score_type
    | "server_move" -> Server_move_type
    | "server_presence" -> Server_presence_type
    | "server_move_info" -> Server_move_info_type
    | "server_shuffles" -> Server_shuffles_type
    | "client_move" -> Client_move_type
    | "client_shuffle" -> Client_shuffle_type
    | "client_start_game" -> Client_start_game_type
    | ts -> raise (Invalid_argument ("Unknown message type string: " ^ ts))

type player_data = {
    player_id: int;
    name: string;
    presence: bool;
    score: int;
    shuffles: int;
}

let make_player_data player_id name presence score shuffles = {
    player_id;
    name;
    presence;
    score;
    shuffles;
}

type name_data = {
    player_id: int;
    name: string;
}

let make_name_data player_id name = {
    player_id;
    name;
}

type board_card_data = {
    idx: int;
    card: Card.t option;
}

let make_board_card_data idx card_id = {
    idx;
    card = Card.of_int_opt card_id;
}

let make_board_cards_data l = List.mapi make_board_card_data l

let board_card_data_to_string d =
    Printf.sprintf "(%d, %d)" d.idx (Card.to_int_opt d.card)

type card_data = {
    idx: int;
    card: Card.t
}

let make_card_data idx card_id = {
    idx;
    card = Card.of_int card_id;
}

let card_data_to_string d =
    Printf.sprintf "(%d, %d)" d.idx (Card.to_int d.card)

type move_data = {
    card0: card_data;
    card1: card_data;
    card2: card_data;
}

let make_move_data (idx0, card0_id) (idx1, card1_id) (idx2, card2_id) = {
    card0 = make_card_data idx0 card0_id;
    card1 = make_card_data idx1 card1_id;
    card2 = make_card_data idx2 card2_id;
}

type game_update_data = {
    card_idx: int;
    status: Game_status.t;
    theme: Theme.t;
    dim0: int;
    dim1: int;
}

let make_game_update_data card_idx status theme dim0 dim1 = {
    card_idx;
    status = Game_status.of_string status;
    theme = Theme.of_string theme;
    dim0;
    dim1;
}

type game_data = {
    player_data: player_data list;
    board_card_data: board_card_data list;
    game_update_data: game_update_data;
}

let make_game_data player_data board_card_data game_update_data = {
    player_data;
    board_card_data;
    game_update_data;
}

type score_data = {
    player_id: int;
    score: int;
}

let make_score_data player_id score = {
    player_id;
    score;
}

type move_info_data = {
    score_data: score_data;
    move_data: move_data;
}

let make_move_info_data score_data move_data = {
    score_data;
    move_data;
}

type presence_data = {
    player_id: int;
    presence: bool;
}

let make_presence_data player_id presence = {
    player_id;
    presence;
}

type shuffle_data = {
    player_id: int;
    shuffles: int;
}

let make_shuffles_data player_id shuffles = {
    player_id;
    shuffles;
}

type token = string

let token_of_string (s: string): token = s
let token_to_string (t: token): string = t

type t =
    | Server_game of game_data
    | Server_player of player_data
    | Server_name of name_data
    | Server_card of card_data
    | Server_board_card of board_card_data
    | Server_game_update of game_update_data
    | Server_score of score_data
    | Server_move of move_data
    | Server_presence of presence_data
    | Server_move_info of move_info_data
    | Server_shuffles of shuffle_data
    | Client_move of (token * move_data)
    | Client_shuffle of token
    | Client_start_game of token

let make_server_game player_data card_data game_update_data =
    Server_game (make_game_data player_data card_data game_update_data)

let make_server_player player_id name presence score shuffles =
    Server_player (make_player_data player_id name presence score shuffles)

let make_server_name player_id name =
    Server_name (make_name_data player_id name)

let make_server_card idx card_id =
    Server_card (make_card_data idx card_id)

let make_server_board_card idx card_id =
    Server_board_card (make_board_card_data idx card_id)

let make_server_game_update card_idx status theme dim0 dim1 =
    Server_game_update (make_game_update_data card_idx status theme dim0 dim1)

let make_server_score player_id score =
    Server_score (make_score_data player_id score)

let make_server_move (idx0, card0_id) (idx1, card1_id) (idx2, card2_id) =
    Server_move (make_move_data (idx0, card0_id) (idx1, card1_id) (idx2, card2_id))

let make_server_presence player_id presence =
    Server_presence (make_presence_data player_id presence)

let make_server_move_info score_data move_data =
    Server_move_info (make_move_info_data score_data move_data)

let make_server_shuffles player_id shuffles =
    Server_shuffles (make_shuffles_data player_id shuffles)

let make_client_move token move_data =
    Client_move (token, move_data)

let make_client_shuffle token = Client_shuffle token

let make_client_start_game token = Client_start_game token

let card_opt_to_string = function
    | Some c -> Card.to_string c
    | None -> "{NONE}"

let rec to_string = function
    | Server_game d ->
        let player_strs = List.map (fun pd -> Server_player pd |> to_string) d.player_data in
        let players = String.concat ", " player_strs in
        let board_card_strs = List.map (fun c -> Server_board_card c |> to_string) d.board_card_data in
        let board_cards = String.concat ", " board_card_strs in
        let game_update = to_string @@ Server_game_update d.game_update_data in
        Printf.sprintf "<message (%s) game_update=%s players=[%s] board_cards=[%s]>"
            (message_type_to_string Server_game_type) game_update players board_cards
    | Server_player d ->
        Printf.sprintf "<message (%s) id=%d name='%s' presence=%b score=%d shuffles=%d>"
            (message_type_to_string Server_player_type) d.player_id d.name d.presence d.score d.shuffles
    | Server_name d ->
        Printf.sprintf "<message (%s) player_id=%d name='%s'>"
            (message_type_to_string Server_name_type) d.player_id d.name
    | Server_card d ->
        Printf.sprintf "<message (%s) idx=%d card=%s>"
            (message_type_to_string Server_card_type) d.idx (Card.to_string d.card)
    | Server_board_card d ->
        Printf.sprintf "<message (%s) idx=%d board_card=%s>"
            (message_type_to_string Server_board_card_type) d.idx (card_opt_to_string d.card)
    | Server_game_update d ->
        Printf.sprintf "<message (%s) card_idx=%d status=%s theme=%s dim0=%d dim1=%d>"
            (message_type_to_string Server_game_update_type) d.card_idx (Game_status.to_string d.status) (Theme.to_string d.theme) d.dim0 d.dim1
    | Server_score d ->
        Printf.sprintf "<message (%s) player_id=%d score=%d>"
            (message_type_to_string Server_score_type) d.player_id d.score
    | Server_move d ->
        Printf.sprintf "<message (%s) card0=%s card1=%s card2=%s>"
            (message_type_to_string Server_move_type) (card_data_to_string d.card0) (card_data_to_string d.card1) (card_data_to_string d.card2)
    | Server_presence d ->
        Printf.sprintf "<message (%s) player_id=%d presence=%b>"
            (message_type_to_string Server_presence_type) d.player_id d.presence
    | Server_move_info d ->
        let score = to_string @@ Server_score d.score_data in
        let move = to_string @@ Server_move d.move_data in
        Printf.sprintf "<message (%s) score=%s move=%s>"
            (message_type_to_string Server_move_info_type) score move
    | Server_shuffles d ->
        Printf.sprintf "<message (%s) player_id=%d shuffles=%d>"
            (message_type_to_string Server_shuffles_type) d.player_id d.shuffles
    | Client_move (_, d) ->
        Printf.sprintf "<message (%s) %s, %s, %s>"
            (message_type_to_string Client_move_type)
            (card_data_to_string d.card0)
            (card_data_to_string d.card1)
            (card_data_to_string d.card2)
    | Client_shuffle _ -> Printf.sprintf "<message (%s)>" (message_type_to_string Client_shuffle_type)
    | Client_start_game _ -> Printf.sprintf "<message (%s)>" (message_type_to_string Client_start_game_type)

module type CONVERT = sig
    val to_json : t -> string
    val of_json : string -> t
end
