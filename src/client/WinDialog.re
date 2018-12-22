let component = ReasonReact.statelessComponent("WinDialog");

[%mui.withStyles
  "StyledDialog"({
    root: ReactDOMRe.Style.make(~color="red", ()),
    container: ReactDOMRe.Style.make(~display="flex", ~flexWrap="wrap", ()),
  })
];

let titleAndMessage = maybeWinner =>
  switch (maybeWinner) {
  | Some((winner: Messages.player_data)) =>
    let player_id = winner.player_id;
    let (title, message) =
      if (ClientUtil.is_current_player(player_id)) {
        ("You won!", "Congratulations, want to play again?");
      } else {
        let name = ClientUtil.get_player_name(winner);
        (name ++ " won!", "Bummer, care for a rematch?");
      };
    (title, message);
  | None => ("Nobody won!?!", "Try again?")
  };

let make = (_children, ~maybeGameId, ~maybeWinner) => {
  ...component,
  render: _self => {
    let (title, message) = titleAndMessage(maybeWinner);
    MaterialUi.(
      <StyledDialog
        render=(
          _classes =>
            <Dialog open_=true>
              <DialogTitle> (ReasonReact.string(title)) </DialogTitle>
              <form noValidate=false autoComplete="off">
                <DialogContent>
                  <DialogContentText> (ReasonReact.string(message)) </DialogContentText>
                </DialogContent>
                <DialogActions>
                  <Button variant=`Contained href="/">
                    (ReasonReact.string("Home"))
                    <Icon> (ReasonReact.string("home")) </Icon>
                  </Button>
                  <PlayAgainButton maybeGameId />
                </DialogActions>
              </form>
            </Dialog>
        )
      />
    );
  },
};
