open Belt;

type action =
  | ToggleDialog;

type state = {dialogOpen: bool};

let component = ReasonReact.reducerComponent("PlayerScores");

let playerDataRow =
    (~palette: Theme.palette, ~open_, ~toggleDialogState, ~sendMessage, player_data: Messages.player_data) => {
  let isCurrentPlayer = ClientUtil.is_current_player(player_data.player_id);
  let pid = string_of_int(player_data.player_id);
  let name = ClientUtil.get_player_name(player_data);
  let nameStyle =
    if (player_data.presence) {
      if (isCurrentPlayer) {
        ReactDOMRe.Style.make(~fontWeight="bold", ~color=palette.primary, ());
      } else {
        ReactDOMRe.Style.make();
      };
    } else {
      ReactDOMRe.Style.make(~color="lightgray", ());
    };
  let toggleDialogEvt = evt => {
    ReactEvent.Synthetic.preventDefault(evt);
    toggleDialogState();
  };
  let nameContent =
    if (isCurrentPlayer) {
      <div>
        <a href="#" onClick=toggleDialogEvt> (ReasonReact.string(name)) </a>
        <NameDialog open_ onCloseState=toggleDialogState onCloseEvt=toggleDialogEvt currentName=name sendMessage />
      </div>;
    } else {
      ReasonReact.string(name);
    };
  MaterialUi.(
    <TableRow key=pid>
      <TableCell style=nameStyle> nameContent </TableCell>
      <TableCell numeric=true> (ReasonReact.string(string_of_int(player_data.score))) </TableCell>
    </TableRow>
  );
};

let make = (_children, ~players, ~palette, ~sendMessage) => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | ToggleDialog => ReasonReact.Update({dialogOpen: ! state.dialogOpen})
    },
  initialState: () => {dialogOpen: false},
  render: self => {
    let toggleDialogState = () => self.ReasonReact.send(ToggleDialog);
    let playerItems =
      List.toArray(
        List.map(players, playerDataRow(~palette, ~open_=self.state.dialogOpen, ~toggleDialogState, ~sendMessage)),
      );
    MaterialUi.(
      <Paper>
        <Table padding=`Dense>
          <TableHead>
            <TableRow>
              <TableCell> (ReasonReact.string("Name")) </TableCell>
              <TableCell> (ReasonReact.string("Score")) </TableCell>
            </TableRow>
          </TableHead>
          <TableBody> (ReasonReact.array(playerItems)) </TableBody>
        </Table>
      </Paper>
    );
  },
};
