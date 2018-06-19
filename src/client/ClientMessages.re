open Messages;

module ClientMessageConverter: CONVERT = {
  let to_json = x => {
    open! Json.Encode;
    let rec aux =
      fun
      | Game_data(d) => {
          let player_data = List.map(pd => aux(Player_data(pd)), d.player_data) |> Array.of_list;
          let board_data = make_board_cards(d.board_data) |> Array.map(aux);
          let game_update = aux(Game_update(d.game_update));
          object_([
            (type_key, string(message_type_to_string(Game_data_type))),
            (player_data_key, jsonArray(player_data)),
            (board_data_key, jsonArray(board_data)),
            (game_update_key, game_update)
          ]);
        }
      | Player_data(d) =>
        object_([
          (type_key, string(message_type_to_string(Player_data_type))),
          (player_id_key, int(d.player_id)),
          (name_key, string(d.name)),
          (presence_key, bool(d.presence)),
          (score_key, int(d.score)),
          (shuffles_key, int(d.shuffles)),
        ])
      | Player_name(d) =>
        object_([
          (type_key, string(message_type_to_string(Player_name_type))),
          (player_id_key, int(d.player_id)),
          (name_key, string(d.name)),
        ])
      | Board_card(d) =>
        object_([
          (type_key, string(message_type_to_string(Board_card_type))),
          (idx_key, int(d.idx)),
          (card_id_key, int(Card.to_int_opt(d.card))),
        ])
      | Game_update(d) =>
        object_([
          (type_key, string(message_type_to_string(Game_update_type))),
          (card_idx_key, int(d.card_idx)),
          (status_key, string(game_status_data_to_string(d.status))),
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
      | Move_data(d) =>
        object_([
          (type_key, string(message_type_to_string(Move_data_type))),
          (score_key, aux(Score(d.score))),
          (previous_move_key, aux(Previous_move(d.previous_move)))
        ])
      | Shuffles(d) =>
        object_([
          (type_key, string(message_type_to_string(Shuffles_type))),
          (player_id_key, int(d.player_id)),
          (shuffles_key, int(d.shuffles))
        ]);
    aux(x) |> Json.stringify;
  };
  let player_data_decoder = json => {
    open! Json.Decode;
    make_player_data(
      json |> field(player_id_key, int),
      json |> field(name_key, string),
      json |> field(presence_key, bool),
      json |> field(score_key, int),
      json |> field(shuffles_key, int)
    )
  };
  let board_card_data_decoder = json => {
    open! Json.Decode;
    make_board_card_data(
        json |> field(idx_key, int),
        json |> field(card_id_key, int)
    )
  };
  let board_card_decoder = json => {
    open! Json.Decode;
    json |> field(card_id_key, int) |> Card.of_int_opt
  };
  let game_update_data_decoder = json => {
    open! Json.Decode;
    make_game_update_data(
        json |> field(status_key, string),
        json |> field(card_idx_key, int)
    )
  };
  let player_name_data_decoder = json => {
    open! Json.Decode;
    make_player_name_data(
      json |> field(player_id_key, int),
      json |> field(name_key, string)
    )
  };
  let score_data_decoder = json => {
    open! Json.Decode;
    make_score_data(
      json |> field(player_id_key, int),
      json |> field(score_key, int)
    )
  };
  let previous_move_data_decoder = json => {
    open! Json.Decode;
    make_previous_move_data(
      json |> field(card0_id_key, int),
      json |> field(card1_id_key, int),
      json |> field(card2_id_key, int)
    )
  };
  let player_presence_data_decoder = json => {
    open! Json.Decode;
    make_player_presence_data(
      json |> field(player_id_key, int),
      json |> field(presence_key, bool)
    )
  };
  let shuffles_data_decoder = json => {
    open! Json.Decode;
    make_shuffles_data(
      json |> field(player_id_key, int),
      json |> field(shuffles_key, int)
    )
  };
  let of_json = str => {
    open! Json.Decode;
    let rec aux = json => {
      let msgType = json |> field(type_key, string) |> message_type_of_string;
      switch (msgType) {
      | Game_data_type =>
        let player_data = json |> field(player_data_key, array(player_data_decoder)) |> Array.to_list;
        let board_data = json |> field(board_data_key, array(board_card_decoder));
        let game_update = json |> field(game_update_key, game_update_data_decoder);
        make_game_data(player_data, board_data, game_update);
      | Player_data_type =>
        Player_data(player_data_decoder(json))
      | Player_name_type =>
        Player_name(player_name_data_decoder(json))
      | Board_card_type => Board_card(board_card_data_decoder(json))
      | Game_update_type => Game_update(game_update_data_decoder(json))
      | Score_type => Score(score_data_decoder(json))
      | Previous_move_type => Previous_move(previous_move_data_decoder(json))
      | Player_presence_type => Player_presence(player_presence_data_decoder(json))
      | Move_data_type =>
        let score = json |> field(score_key, score_data_decoder);
        let previous_move = json |> field(previous_move_key, previous_move_data_decoder);
        make_move_data(score, previous_move)
      | Shuffles_type => Shuffles(shuffles_data_decoder(json))
      };
    };
    let json = Json.parseOrRaise(str);
    aux(json);
  };
};
