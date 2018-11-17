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
        ReactDOMRe.Style.make(~fontWeight="bold", ());
      };
    } else {
      ReactDOMRe.Style.make(~color="gray", ());
    };
  let scoreStyle = ReactDOMRe.Style.make(~width="100%", ~textAlign="right", ());
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
    <ListItem key=pid>
      <ListItemText>
        <span style=scoreStyle> (ReasonReact.string(string_of_int(player_data.score))) </span>
      </ListItemText>
      <ListItemText> <span style=nameStyle> nameContent </span> </ListItemText>
    </ListItem>
  );
};

let comparePlayers = (p0: Messages.player_data, p1: Messages.player_data) =>
  if (p0.score == p1.score) {
    compare(p0.shuffles, p1.shuffles);
  } else {
    compare(p1.score, p0.score);
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
    let sortedPlayers = List.sort(players, comparePlayers);
    let playerItems =
      List.map(
        sortedPlayers,
        playerDataRow(~palette, ~open_=self.state.dialogOpen, ~toggleDialogState, ~sendMessage),
      );
    <MaterialUi.List> (ReasonReact.array(List.toArray(playerItems))) </MaterialUi.List>;
  },
};
