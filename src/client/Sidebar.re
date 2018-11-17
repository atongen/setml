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

[%mui.withStyles
  "StyledSidebar"({
    root: ReactDOMRe.Style.make(~flexGrow="1", ()),
    list: ReactDOMRe.Style.make(~listStyle="none", ~padding="0px", ~marginRight="1em", ()),
    playerScores: ReactDOMRe.Style.make(),
  })
];

let menuItems = () =>
  List.map(
    theme => {
      let t = Theme.to_string(theme);
      <MaterialUi.MenuItem key=t value=(`String(t))>
        (ReasonReact.string(Theme.to_human_string(theme)))
      </MaterialUi.MenuItem>;
    },
    [Theme.Classic, Theme.Open_source],
  );

let make = (_children, ~rect, ~boardCards, ~players, ~game: Messages.game_update_data, ~sendMessage) => {
  let setsOnBoard = Messages_util.board_cards_count_sets(boardCards);
  let cardsRemaining = 81 - game.card_idx + Messages_util.board_cards_count(boardCards);
  let buttonStyle = ReactDOMRe.Style.make(~display="block", ~margin="1em", ());
  let button = makeButton(game.status, setsOnBoard, sendMessage, buttonStyle);
  let palette = Theme.palette(game.theme);
  let themeName = Theme.to_string(game.theme);
  let themeChange = (evt, _el) => {
    let s = ReactEvent.Form.target(evt)##value;
    sendMessage(ClientUtil.make_theme_msg(Theme.of_string(s)));
  };
  let gameUrl =
    switch (ClientUtil.game_url()) {
    | Some(url) => url
    | None => "http://www.example.com"
    };
  {
    ...component,
    render: _self =>
      <section id="sidebar" style=(ClientUtil.rectToStyle(rect, ()))>
        <StyledSidebar
          render=(
            classes =>
              MaterialUi.(
                <div className=classes.root>
                  <Grid container=true>
                    <Grid item=true> (ReasonReact.string("SetML")) </Grid>
                    <Grid item=true> <a href=gameUrl> (ReasonReact.string(gameUrl)) </a> </Grid>
                    <Grid item=true> button </Grid>
                    <Grid item=true>
                      <ul className=classes.list>
                        <li>
                          (
                            ReasonReact.string(
                              string_of_int(setsOnBoard) ++ " " ++ simplePlural("set", setsOnBoard) ++ " on board",
                            )
                          )
                        </li>
                        <li> (ReasonReact.string(string_of_int(cardsRemaining) ++ " cards remaining")) </li>
                      </ul>
                    </Grid>
                    <Grid item=true>
                      <Paper className=classes.playerScores> <PlayerScores players palette sendMessage /> </Paper>
                    </Grid>
                    <Grid item=true>
                      <form autoComplete="off">
                        <FormControl>
                          <InputLabel> (ReasonReact.string("Theme")) </InputLabel>
                          <Select value=(`String(themeName)) onChange=themeChange> (menuItems()) </Select>
                        </FormControl>
                      </form>
                    </Grid>
                  </Grid>
                </div>
              )
          )
        />
      </section>,
  };
};
