let component = ReasonReact.statelessComponent("WinDialog");

[%mui.withStyles
  "StyledDialog"({
    root: ReactDOMRe.Style.make(~color="red", ()),
    container: ReactDOMRe.Style.make(~display="flex", ~flexWrap="wrap", ()),
  })
];

let titleAndMessage = maybeWinner =>
  switch (maybeWinner) {
  | Some(winner) =>
    let title = ClientUtil.get_player_name(winner) ++ " won!";
    let message =
      if (ClientUtil.is_current_player(winner.player_id)) {
        "Congratulations! I bet you can't do that again!";
      } else {
        "Ooh, bummer. You better go for a rematch!";
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
                <DialogActions> <PlayAgainButton maybeGameId /> </DialogActions>
              </form>
            </Dialog>
        )
      />
    );
  },
};
