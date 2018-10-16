open Belt;

let component = ReasonReact.statelessComponent("Sidebar");

let avatarLetter = name =>
  if (String.length(name) > 0) {
    let c = Char.uppercase(name.[0]);
    String.make(1, c);
  } else {
    "P";
  };

let playerDataToLi = (player_data: Messages.player_data) => {
  let pid = string_of_int(player_data.player_id);
  let name = ClientUtil.get_player_name(player_data);
  <MaterialUi.ListItem key=pid>
    <MaterialUi.ListItemAvatar>
      <MaterialUi.Avatar> (ReasonReact.string(avatarLetter(name))) </MaterialUi.Avatar>
    </MaterialUi.ListItemAvatar>
    <MaterialUi.ListItemText>
      (
        ReasonReact.string(
          Printf.sprintf(
            "%s - Present: %B, Score: %d, Shuffles: %d",
            name,
            player_data.presence,
            player_data.score,
            player_data.shuffles,
          ),
        )
      )
    </MaterialUi.ListItemText>
  </MaterialUi.ListItem>;
};

let makeButton = (gameStatus, setsOnBoard, sendMessage) =>
  switch (gameStatus) {
  | Game_status.New =>
    let startClick = _evt => sendMessage(ClientUtil.make_start_game_msg());
    <MaterialUi.Button onClick=startClick variant=`Contained>
      (ReasonReact.string("Start"))
      <MaterialUi.Icon> (ReasonReact.string("play_arrow")) </MaterialUi.Icon>
    </MaterialUi.Button>;
  | Game_status.Started =>
    let shuffleClick = _evt => sendMessage(ClientUtil.make_shuffle_msg());
    let variant =
      if (setsOnBoard == 0) {
        `Contained;
      } else {
        `Outlined;
      };
    <MaterialUi.Button onClick=shuffleClick variant>
      (ReasonReact.string("Shuffle"))
      <MaterialUi.Icon> (ReasonReact.string("shuffle")) </MaterialUi.Icon>
    </MaterialUi.Button>;
  | Game_status.Complete => ReasonReact.null
  };

let make = (_children, ~rect, ~boardCards, ~players, ~game: Messages.game_update_data, ~previousMove, ~sendMessage) => {
  let setsOnBoard = Messages_util.board_cards_count_sets(boardCards);
  let cardsRemaining = 81 - game.card_idx + Messages_util.board_cards_count(boardCards);
  let pmove =
    switch (previousMove) {
    | Some(move) =>
      <div id="previous-move">
        <h2> (ReasonReact.string("Previous Move")) </h2>
        <ul>
          <li key=(string_of_int(Card.to_int(move.Messages.card0.card)))>
            (ReasonReact.string(Theme.card_to_string(game.theme, move.card0.card)))
          </li>
          <li key=(string_of_int(Card.to_int(move.Messages.card1.card)))>
            (ReasonReact.string(Theme.card_to_string(game.theme, move.card1.card)))
          </li>
          <li key=(string_of_int(Card.to_int(move.Messages.card2.card)))>
            (ReasonReact.string(Theme.card_to_string(game.theme, move.card2.card)))
          </li>
        </ul>
      </div>
    | None => ReasonReact.null
    };
  let sortedPlayers = List.sort(players, (p0: Messages.player_data, p1) => compare(p1.score, p0.score));
  let playerItems = List.map(sortedPlayers, playerDataToLi);
  let button = makeButton(game.status, setsOnBoard, sendMessage);
  {
    ...component,
    render: _self =>
      <section id="sidebar" style=(Rect.toStyle(rect))>
        <div id="header">
          button
          <ul>
            <li> (ReasonReact.string(string_of_int(setsOnBoard) ++ " sets on board")) </li>
            <li> (ReasonReact.string(string_of_int(cardsRemaining) ++ " cards remaining")) </li>
          </ul>
        </div>
        <div id="scores">
          <h2> (ReasonReact.string("Score")) </h2>
          <MaterialUi.List> (ReasonReact.array(List.toArray(playerItems))) </MaterialUi.List>
        </div>
        pmove
      </section>,
  };
};
