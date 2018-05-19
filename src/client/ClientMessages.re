open Messages;

module ClientMessageConverter: CONVERT = {
  let to_json = x => {
    open! Json.Encode;
    let rec aux =
      fun
      | Scoreboard(d) => {
          let players =
            List.map(
              (p: scoreboard_player_data) =>
                object_([
                  (player_id_key, int(p.player_id)),
                  (player_name_key, string(p.name)),
                  (presence_key, bool(p.presence)),
                  (score_key, int(p.score)),
                ]),
              d.players,
            )
            |> Array.of_list;
          let board = List.map(c => int(Card.to_int(c)), d.board) |> Array.of_list;
          object_([
            (type_key, string(message_type_to_string(Scoreboard_type))),
            (players_key, jsonArray(players)),
            (board_key, jsonArray(board)),
            (game_status_key, string(game_status_data_to_string(d.game_status))),
          ]);
        }
      | Player_name(d) =>
        object_([
          (type_key, string(message_type_to_string(Player_name_type))),
          (player_id_key, int(d.player_id)),
          (player_name_key, string(d.name)),
        ])
      | Board_card(d) =>
        object_([
          (type_key, string(message_type_to_string(Board_card_type))),
          (idx_key, int(d.idx)),
          (card_id_key, int(Card.to_int(d.card))),
        ])
      | Game_card_idx(d) =>
        object_([
          (type_key, string(message_type_to_string(Game_card_idx_type))),
          (card_idx_key, int(d.card_idx)),
        ])
      | Game_status(d) =>
        object_([
          (type_key, string(message_type_to_string(Game_status_type))),
          (status_key, string(game_status_data_to_string(d))),
        ])
      | Score(d) =>
        object_([
          (type_key, string(message_type_to_string(Score_type))),
          (player_id_key, int(d.player_id)),
          (score_key, int(d.score)),
        ])
      | Previous_move(d) =>
        object_([
          (type_key, string(message_type_to_string(Previous_move_type))),
          (card0_id_key, int(Card.to_int(d.card0))),
          (card1_id_key, int(Card.to_int(d.card1))),
          (card2_id_key, int(Card.to_int(d.card2))),
        ])
      | Player_presence(d) =>
        object_([
          (type_key, string(message_type_to_string(Player_presence_type))),
          (player_id_key, int(d.player_id)),
          (presence_key, bool(d.presence)),
        ])
      | Batch(d) => {
          let messages = List.map(aux, d) |> Array.of_list;
          object_([(type_key, string(message_type_to_string(Batch_type))), (messages_key, jsonArray(messages))]);
        };
    aux(x) |> Json.stringify;
  };
  let of_json = str => {
    open! Json.Decode;
    let rec aux = json => {
      let msgType = json |> field(type_key, string) |> message_type_of_string;
      switch (msgType) {
      | Scoreboard_type =>
        let players_decoder = json =>
          make_scoreboard_player_data(
            json |> field(player_id_key, int),
            json |> field(player_name_key, string),
            json |> field(presence_key, bool),
            json |> field(score_key, int),
          );
        let players = json |> field(players_key, array(players_decoder)) |> Array.to_list;
        let board = json |> field(board_key, array(int)) |> Array.to_list;
        let game_status = json |> field(game_status_key, string) |> game_status_data_of_string;
        make_scoreboard(players, List.map(Card.of_int, board), game_status);
      | Player_name_type =>
        make_player_name(json |> field(player_id_key, int), json |> field(player_name_key, string))
      | Board_card_type => make_board_card(json |> field(idx_key, int), json |> field(card_id_key, int))
      | Game_card_idx_type => make_game_card_idx(json |> field(card_idx_key, int))
      | Game_status_type => make_game_status(json |> field(status_key, string))
      | Score_type => make_score(json |> field(player_id_key, int), json |> field(score_key, int))
      | Previous_move_type =>
        make_previous_move(
          json |> field(card0_id_key, int),
          json |> field(card1_id_key, int),
          json |> field(card2_id_key, int),
        )
      | Player_presence_type =>
        make_player_presence(json |> field(player_id_key, int), json |> field(presence_key, bool))
      | Batch_type =>
        let messages = json |> field(messages_key, array(aux)) |> Array.to_list;
        make_batch(messages);
      };
    };
    let json = Json.parseOrRaise(str);
    aux(json);
  };
};
