let component = ReasonReact.statelessComponent("Sidebar");

let buttonStyle = color =>
  switch (color) {
  | Some(backgroundColor) => ReactDOMRe.Style.make(~display="block", ~margin="1em", ~backgroundColor, ())
  | None => ReactDOMRe.Style.make(~display="block", ~margin="1em", ())
  };

let makeButton = (game: Messages.game_update_data, setsOnBoard, sendMessage) =>
  switch (game.status) {
  | New =>
    let startClick = _evt => sendMessage(ClientUtil.make_start_game_msg());
    let style = buttonStyle(Some("green"));
    MaterialUi.(
      <Button onClick=startClick variant=`Contained style>
        (ReasonReact.string("Start"))
        <Icon> (ReasonReact.string("play_arrow")) </Icon>
      </Button>
    );
  | Started =>
    let shuffleClick = _evt => sendMessage(ClientUtil.make_shuffle_msg());
    let (variant, style) =
      if (setsOnBoard == 0) {
        (`Contained, buttonStyle(Some("yellow")));
      } else {
        (`Outlined, buttonStyle(None));
      };
    MaterialUi.(
      <Button onClick=shuffleClick variant style>
        (ReasonReact.string("Shuffle"))
        <Icon> (ReasonReact.string("shuffle")) </Icon>
      </Button>
    );
  | Complete => <PlayAgainButton maybeGameId=game.next_game_id />
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
  let button = makeButton(game, setsOnBoard, sendMessage);
  let palette = Theme.palette(game.theme);
  let themeName = Theme.to_string(game.theme);
  let themeChange = (evt, _el) => {
    let s = ReactEvent.Form.target(evt)##value;
    sendMessage(ClientUtil.make_theme_msg(Theme.of_string(s)));
  };
  let sortedPlayers = ClientUtil.sortPlayers(players);
  let winDialog =
    switch (game.status) {
    | Complete =>
      let maybeWinner =
        if (List.length(sortedPlayers) > 0) {
          Some(List.hd(sortedPlayers));
        } else {
          None;
        };
      <WinDialog maybeGameId=game.next_game_id maybeWinner />;
    | _ => ReasonReact.null
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
                      <Paper className=classes.playerScores>
                        <PlayerScores players=sortedPlayers palette sendMessage />
                      </Paper>
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
        winDialog
      </section>,
  };
};
