open Messages;

module ClientMessageConverter: CONVERT = {
  let to_json = x => {
    open! Json.Encode;
    switch (x) {
    | Presence(pt) =>
      object_([
        (type_key, string(message_type_to_string(Presence_type))),
        (game_id_key, int(pt.game_id)),
        (player_id_key, int(pt.player_id)),
        (player_name_key, string(pt.player_name)),
        (value_key, bool(pt.value)),
      ])
      |> Json.stringify
    };
  };
  let of_json = str => {
    let json = Json.parseOrRaise(str);
    open! Json.Decode;
    let msgType =
      json |> field(type_key, Json.Decode.string) |> message_type_of_string;
    switch (msgType) {
    | Presence_type =>
      make_presence(
        json |> field(game_id_key, int),
        json |> field(player_id_key, int),
        json |> field(player_name_key, string),
        json |> field(value_key, bool),
      )
    };
  };
};
