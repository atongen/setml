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
      ReasonReact.stringToElement(
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
  let shuffleClick = evt => sendMessage(ClientUtil.make_shuffle_msg());
  let gameUrl = get_game_url();
  let setsOnBoard = Messages_util.board_cards_count_sets(boardCards);
  let cardsRemaining = 81 - game.card_idx;
  let pmove =
    switch (previousMove) {
    | Some(move) =>
      <div id="previous-move">
        <h2> (ReasonReact.stringToElement("Previous Move")) </h2>
        <ul>
          <li key=(string_of_int(Card.to_int(move.Messages.card0.card)))>
            (ReasonReact.stringToElement(Theme.card_to_string(game.theme, move.card0.card)))
          </li>
          <li key=(string_of_int(Card.to_int(move.Messages.card1.card)))>
            (ReasonReact.stringToElement(Theme.card_to_string(game.theme, move.card1.card)))
          </li>
          <li key=(string_of_int(Card.to_int(move.Messages.card2.card)))>
            (ReasonReact.stringToElement(Theme.card_to_string(game.theme, move.card2.card)))
          </li>
        </ul>
      </div>
    | None => ReasonReact.nullElement
    };
  let playerItems = List.map(players, playerDataToLi);
  {
    ...component,
    render: _self =>
      <section id="sidebar" style=(Rect.toStyle(rect))>
        <div id="header">
          <h1> <a href="/"> (ReasonReact.stringToElement("SetML")) </a> </h1>
          <ul>
            <li>
              (ReasonReact.stringToElement("Game: "))
              <a href=gameUrl> (ReasonReact.stringToElement(gameUrl)) </a>
            </li>
            <li> (ReasonReact.stringToElement("Sets on board: " ++ string_of_int(setsOnBoard))) </li>
            <li> (ReasonReact.stringToElement("Cards remaining: " ++ string_of_int(cardsRemaining))) </li>
          </ul>
          <button onClick=shuffleClick> (ReasonReact.stringToElement("Shuffle!")) </button>
        </div>
        <div id="scores">
          <h2> (ReasonReact.stringToElement("Score")) </h2>
          <ul> (ReasonReact.arrayToElement(List.toArray(playerItems))) </ul>
        </div>
        pmove
      </section>,
  };
};
