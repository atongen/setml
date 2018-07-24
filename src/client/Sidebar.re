open Belt;

let component = ReasonReact.statelessComponent("Sidebar");

let get_game_url = () =>
  switch (ClientUtil.game_url()) {
  | Some(url) => url
  | None => ""
  };

let playerDataToLi = (player_data: Messages.player_data) =>
  <li key=(string_of_int(player_data.player_id))>
    (
      ReasonReact.string(
        Printf.sprintf(
          "%s - Present: %B, Score: %d, Shuffles: %d",
          player_data.name,
          player_data.presence,
          player_data.score,
          player_data.shuffles,
        ),
      )
    )
  </li>;

let make = (_children, ~rect, ~boardCards, ~players, ~game: Messages.game_update_data, ~previousMove, ~sendMessage) => {
  let shuffleClick = _ => sendMessage(ClientUtil.make_shuffle_msg());
  let startClick = _ => sendMessage(ClientUtil.make_start_game_msg());
  let gameUrl = get_game_url();
  let setsOnBoard = Messages_util.board_cards_count_sets(boardCards);
  let cardsRemaining = 81 - game.card_idx;
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
  let playerItems = List.map(players, playerDataToLi);
  {
    ...component,
    render: _self =>
      <section id="sidebar" style=(Rect.toStyle(rect))>
        <div id="header">
          <h1> <a href="/"> (ReasonReact.string("SetML")) </a> </h1>
          <ul>
            <li>
              (ReasonReact.string("Game: "))
              <a href=gameUrl> (ReasonReact.string(gameUrl)) </a>
            </li>
            <li> (ReasonReact.string("Sets on board: " ++ string_of_int(setsOnBoard))) </li>
            <li> (ReasonReact.string("Cards remaining: " ++ string_of_int(cardsRemaining))) </li>
          </ul>
          <MaterialUi.Button onClick=shuffleClick> (ReasonReact.string("Shuffle!")) </MaterialUi.Button>
          <MaterialUi.Button onClick=startClick> (ReasonReact.string("Start!")) </MaterialUi.Button>
        </div>
        <div id="scores">
          <h2> (ReasonReact.string("Score")) </h2>
          <ul> (ReasonReact.array(List.toArray(playerItems))) </ul>
        </div>
        pmove
      </section>,
  };
};
