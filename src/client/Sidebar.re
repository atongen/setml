let component = ReasonReact.statelessComponent("Sidebar");

let makeButton = (game: Messages.game_update_data, setsOnBoard, sendMessage) =>
  switch (game.status) {
  | New =>
    let startClick = _evt => sendMessage(ClientUtil.make_start_game_msg());
    let color = `Primary
    MaterialUi.(
      <Button onClick=startClick variant=`Contained color>
        (ReasonReact.string("Start"))
        <Icon> (ReasonReact.string("play_arrow")) </Icon>
      </Button>
    );
  | Started =>
    let shuffleClick = _evt => sendMessage(ClientUtil.make_shuffle_msg());
    let (variant, color) =
      if (setsOnBoard == 0) {
        (`Contained, `Secondary);
      } else {
        (`Outlined, `Default);
      };
    MaterialUi.(
      <Button onClick=shuffleClick variant color>
        (ReasonReact.string("Shuffle"))
        <Icon> (ReasonReact.string("shuffle")) </Icon>
      </Button>
    );
  | Complete => ReasonReact.null
  };

let simplePlural = (word, num) =>
  if (num == 1) {
    word;
  } else {
    word ++ "s";
  };

let menuItems = () =>
  List.map(
    theme => {
      let t = Theme.to_string(theme);
      <MaterialUi.MenuItem key=t value=(`String(t))>
        (ReasonReact.string(Theme.to_human_string(theme)))
      </MaterialUi.MenuItem>;
    },
    [Theme.Classic, Theme.Open_source, Theme.Hero],
  );

let gridItemStyle = ReactDOMRe.Style.make(~margin="5px", ());

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
    let style=ClientUtil.rectToStyle(rect, ~overflowX="hidden", ~overflowY="auto", ~padding="0", ~margin="0", ~display="block", ~position="fixed", ());
  {
    ...component,
    render: _self =>
      <section id="sidebar" style>
        MaterialUi.(
          <Grid container=true>
            <Grid item=true style=gridItemStyle>
              <Card>
                <CardContent>
                  <Typography>
                    (
                      ReasonReact.string(
                        string_of_int(setsOnBoard) ++ " " ++ simplePlural("set", setsOnBoard) ++ " on board",
                      )
                    )
                  </Typography>
                  <Typography>
                    (ReasonReact.string(string_of_int(cardsRemaining) ++ " cards remaining"))
                  </Typography>
                  button
                </CardContent>
              </Card>
            </Grid>
            <Grid item=true style=gridItemStyle>
              <Paper>
                <PlayerScores players=sortedPlayers palette sendMessage />
              </Paper>
            </Grid>
            <Grid item=true style=gridItemStyle>
              <form autoComplete="off">
                <FormControl>
                  <InputLabel> (ReasonReact.string("Theme")) </InputLabel>
                  <Select value=(`String(themeName)) onChange=themeChange> (menuItems()) </Select>
                </FormControl>
              </form>
            </Grid>
          </Grid>
        )
        winDialog
      </section>,
  };
};
