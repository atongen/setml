let component = ReasonReact.statelessComponent("Sidebar");

let makeButton = (gameStatus, setsOnBoard, sendMessage, style) =>
  switch (gameStatus) {
  | Game_status.New =>
    let startClick = _evt => sendMessage(ClientUtil.make_start_game_msg());
    <MaterialUi.Button onClick=startClick variant=`Contained style>
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
    <MaterialUi.Button onClick=shuffleClick variant style>
      (ReasonReact.string("Shuffle"))
      <MaterialUi.Icon> (ReasonReact.string("shuffle")) </MaterialUi.Icon>
    </MaterialUi.Button>;
  | Game_status.Complete => ReasonReact.null
  };

let simplePlural = (word, num) =>
  if (num == 1) {
    word;
  } else {
    word ++ "s";
  };

[%mui.withStyles "Styles"({
    root: ReactDOMRe.Style.make(~flexGrow="1", ()),
})];

let make = (_children, ~rect, ~boardCards, ~players, ~game: Messages.game_update_data, ~sendMessage) => {
  let setsOnBoard = Messages_util.board_cards_count_sets(boardCards);
  let cardsRemaining = 81 - game.card_idx + Messages_util.board_cards_count(boardCards);
  let buttonStyle = ReactDOMRe.Style.make(~display="block", ~margin="1em", ());
  let button = makeButton(game.status, setsOnBoard, sendMessage, buttonStyle);
  {
    ...component,
    render: _self =>
      <section id="sidebar" style=(Rect.toStyle(rect))>
        <Styles
          render=(
            classes =>
              <div className=classes.root>
                <MaterialUi.Grid container=true>
                  <MaterialUi.Grid item=true> button </MaterialUi.Grid>
                  <MaterialUi.Grid item=true>
                    <ul>
                      <li>
                        (
                          ReasonReact.string(
                            string_of_int(setsOnBoard) ++ " " ++ simplePlural("set", setsOnBoard) ++ " on board",
                          )
                        )
                      </li>
                      <li> (ReasonReact.string(string_of_int(cardsRemaining) ++ " cards remaining")) </li>
                    </ul>
                  </MaterialUi.Grid>
                  <MaterialUi.Grid item=true>
                    <PlayerScores players />
                  </MaterialUi.Grid>
                </MaterialUi.Grid>
              </div>
          )
        />
      </section>,
  };
};
