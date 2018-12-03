open Api_messages;

module ClientApiMessageConverter: CONVERT = {
  let player_games_to_json = player_games => {
    open! Json.Encode;
    jsonArray(
      Array.map(
        player_game =>
          object_([
            (id_key, int(player_game.id)),
            (status_key, string(Game_status.to_string(player_game.status))),
            (card_idx_key, int(player_game.card_idx)),
            (joined_at_key, float(player_game.joined_at)),
          ]),
        Array.of_list(player_games),
      ),
    )
    |> Json.stringify;
  };
  let player_game_decoder = json => {
    open! Json.Decode;
    make_player_game(
      json |> field(id_key, int),
      json |> field(status_key, string) |> Game_status.of_string,
      json |> field(card_idx_key, int),
      json |> field(joined_at_key, float),
    );
  };

  let player_games_of_json = str => {
    open! Json.Decode;
    Json.parseOrRaise(str) |> array(player_game_decoder) |> Array.to_list;
  };
};
