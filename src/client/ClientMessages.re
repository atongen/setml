open Messages;

module ClientMessageConverter: CONVERT = {
  let card_data_to_json = card_data => {
    open! Json.Encode;
    object_([
      (type_key, string(message_type_to_string(Server_card_type))),
      (idx_key, int(card_data.idx)),
      (card_id_key, int(Card.to_int(card_data.card))),
    ]);
  };
  let to_json = x => {
    open! Json.Encode;
    let rec aux =
      fun
      | Server_game(d) => {
          let player_data = List.map(pd => aux(Server_player(pd)), d.player_data) |> Array.of_list;
          let board_card_data = List.map(bc => aux(Server_board_card(bc)), d.board_card_data) |> Array.of_list;
          let game_update = Server_game_update(d.game_update_data);
          object_([
            (type_key, string(message_type_to_string(Server_game_type))),
            (player_data_key, jsonArray(player_data)),
            (board_card_data_key, jsonArray(board_card_data)),
            (game_update_key, aux(game_update)),
          ]);
        }
      | Server_player(d) =>
        object_([
          (type_key, string(message_type_to_string(Server_player_type))),
          (player_id_key, int(d.player_id)),
          (name_key, string(d.name)),
          (presence_key, bool(d.presence)),
          (score_key, int(d.score)),
          (shuffles_key, int(d.shuffles)),
        ])
      | Server_name(d) =>
        object_([
          (type_key, string(message_type_to_string(Server_name_type))),
          (player_id_key, int(d.player_id)),
          (name_key, string(d.name)),
        ])
      | Server_card(d) => card_data_to_json(d)
      | Server_board_card(d) =>
        object_([
          (type_key, string(message_type_to_string(Server_board_card_type))),
          (idx_key, int(d.idx)),
          (card_id_key, int(Card.to_int_opt(d.card))),
        ])
      | Server_game_update(d) =>
        object_([
          (type_key, string(message_type_to_string(Server_game_update_type))),
          (card_idx_key, int(d.card_idx)),
          (status_key, string(Game_status.to_string(d.status))),
          (theme_key, string(Theme.to_string(d.theme))),
          (dim0_key, int(d.dim0)),
          (dim1_key, int(d.dim1)),
        ])
      | Server_score(d) =>
        object_([
          (type_key, string(message_type_to_string(Server_score_type))),
          (player_id_key, int(d.player_id)),
          (score_key, int(d.score)),
        ])
      | Server_move(d) =>
        object_([
          (type_key, string(message_type_to_string(Server_move_type))),
          (card0_key, card_data_to_json(d.card0)),
          (card1_key, card_data_to_json(d.card1)),
          (card2_key, card_data_to_json(d.card2)),
        ])
      | Server_presence(d) =>
        object_([
          (type_key, string(message_type_to_string(Server_presence_type))),
          (player_id_key, int(d.player_id)),
          (presence_key, bool(d.presence)),
        ])
      | Server_move_info(d) =>
        object_([
          (type_key, string(message_type_to_string(Server_move_info_type))),
          (score_data_key, aux(Server_score(d.score_data))),
          (move_data_key, aux(Server_move(d.move_data))),
        ])
      | Server_shuffles(d) =>
        object_([
          (type_key, string(message_type_to_string(Server_shuffles_type))),
          (player_id_key, int(d.player_id)),
          (shuffles_key, int(d.shuffles)),
        ])
      | Client_move((token, d)) =>
        object_([
          (type_key, string(message_type_to_string(Client_move_type))),
          (token_key, string(token_to_string(token))),
          (card0_key, card_data_to_json(d.card0)),
          (card1_key, card_data_to_json(d.card1)),
          (card2_key, card_data_to_json(d.card2)),
        ])
      | Client_shuffle(token) =>
        object_([
          (type_key, string(message_type_to_string(Client_shuffle_type))),
          (token_key, string(token_to_string(token))),
        ])
      | Client_start_game(token) =>
        object_([
          (type_key, string(message_type_to_string(Client_start_game_type))),
          (token_key, string(token_to_string(token))),
        ])
      | Client_name((token, name)) =>
        object_([
          (type_key, string(message_type_to_string(Client_name_type))),
          (token_key, string(token_to_string(token))),
          (name_key, string(name)),
        ])
      | Client_theme((token, theme)) =>
        object_([
          (type_key, string(message_type_to_string(Client_theme_type))),
          (token_key, string(token_to_string(token))),
          (theme_key, string(Theme.to_string(theme))),
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
      json |> field(shuffles_key, int),
    );
  };
  let score_data_decoder = json => {
    open! Json.Decode;
    make_score_data(json |> field(player_id_key, int), json |> field(score_key, int));
  };
  let card_data_decoder = json => {
    open! Json.Decode;
    make_card_data(json |> field(idx_key, int), json |> field(card_id_key, int));
  };
  let board_card_data_decoder = json => {
    open! Json.Decode;
    make_board_card_data(json |> field(idx_key, int), json |> field(card_id_key, int));
  };
  let move_data_decoder = json => {
    open! Json.Decode;
    {
      card0: json |> field(card0_key, card_data_decoder),
      card1: json |> field(card1_key, card_data_decoder),
      card2: json |> field(card2_key, card_data_decoder),
    };
  };
  let game_update_data_decoder = json => {
    open! Json.Decode;
    make_game_update_data(
      json |> field(card_idx_key, int),
      json |> field(status_key, string),
      json |> field(theme_key, string),
      json |> field(dim0_key, int),
      json |> field(dim1_key, int),
    );
  };
  let name_data_decoder = json => {
    open! Json.Decode;
    make_name_data(json |> field(player_id_key, int), json |> field(name_key, string));
  };
  let presence_data_decoder = json => {
    open! Json.Decode;
    make_presence_data(json |> field(player_id_key, int), json |> field(presence_key, bool));
  };
  let shuffles_data_decoder = json => {
    open! Json.Decode;
    make_shuffles_data(json |> field(player_id_key, int), json |> field(shuffles_key, int));
  };
  let of_json = str => {
    open! Json.Decode;
    let aux = json => {
      let msgType = json |> field(type_key, string) |> message_type_of_string;
      switch (msgType) {
      | Server_game_type =>
        let player_data = json |> field(player_data_key, array(player_data_decoder)) |> Array.to_list;
        let board_card_data = json |> field(board_card_data_key, array(board_card_data_decoder)) |> Array.to_list;
        let game_update = json |> field(game_update_key, game_update_data_decoder);
        make_server_game(player_data, board_card_data, game_update);
      | Server_player_type => Server_player(player_data_decoder(json))
      | Server_name_type => Server_name(name_data_decoder(json))
      | Server_card_type => Server_card(card_data_decoder(json))
      | Server_board_card_type => Server_board_card(board_card_data_decoder(json))
      | Server_game_update_type => Server_game_update(game_update_data_decoder(json))
      | Server_score_type => Server_score(score_data_decoder(json))
      | Server_move_type => Server_move(move_data_decoder(json))
      | Server_presence_type => Server_presence(presence_data_decoder(json))
      | Server_move_info_type =>
        let score_data = json |> field(score_data_key, score_data_decoder);
        let move_data = json |> field(move_data_key, move_data_decoder);
        Server_move_info({score_data, move_data});
      | Server_shuffles_type => Server_shuffles(shuffles_data_decoder(json))
      | Client_move_type =>
        let token = json |> field(token_key, string) |> token_of_string;
        let move_data = move_data_decoder(json);
        Client_move((token, move_data));
      | Client_shuffle_type =>
        let token = json |> field(token_key, string) |> token_of_string;
        Client_shuffle(token);
      | Client_start_game_type =>
        let token = json |> field(token_key, string) |> token_of_string;
        Client_start_game(token);
      | Client_name_type =>
        let token = json |> field(token_key, string) |> token_of_string;
        let name = json |> field(name_key, string);
        Client_name((token, name));
      | Client_theme_type =>
        let token = json |> field(token_key, string) |> token_of_string;
        let theme = json |> field(theme_key, string) |> Theme.of_string;
        Client_theme((token, theme));
      };
    };
    let json = Json.parseOrRaise(str);
    aux(json);
  };
};
