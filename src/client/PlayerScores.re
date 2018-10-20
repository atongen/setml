open Belt;

let component = ReasonReact.statelessComponent("PlayerScores");

let avatarLetter = name =>
  if (String.length(name) > 0) {
    let c = Char.uppercase(name.[0]);
    String.make(1, c);
  } else {
    "P";
  };

let playerDataRow = (player_data: Messages.player_data) => {
  let pid = string_of_int(player_data.player_id);
  let name = ClientUtil.get_player_name(player_data);
  let nameStyle =
    if (player_data.presence) {
      ReactDOMRe.Style.make(~fontWeight="bold", ());
    } else {
      ReactDOMRe.Style.make(~color="gray", ());
    };
  let scoreStyle = ReactDOMRe.Style.make(~width="100%", ~textAlign="right", ());
  <MaterialUi.ListItem key=pid>
    <MaterialUi.ListItemAvatar>
      <MaterialUi.Avatar> (ReasonReact.string(avatarLetter(name))) </MaterialUi.Avatar>
    </MaterialUi.ListItemAvatar>
    <MaterialUi.ListItemText>
      <span style=nameStyle> (ReasonReact.string(name)) </span>
    </MaterialUi.ListItemText>
    <MaterialUi.ListItemText>
      <span style=scoreStyle> (ReasonReact.string(string_of_int(player_data.score))) </span>
    </MaterialUi.ListItemText>
  </MaterialUi.ListItem>;
};

let comparePlayers = (p0: Messages.player_data, p1: Messages.player_data) =>
  if (p0.score == p1.score) {
    compare(p0.shuffles, p1.shuffles);
  } else {
    compare(p1.score, p0.score);
  };

let make = (_children, ~players) => {
  let sortedPlayers = List.sort(players, comparePlayers);
  let playerItems = List.map(sortedPlayers, playerDataRow);
  {
    ...component,
    render: _self => <MaterialUi.List> (ReasonReact.array(List.toArray(playerItems))) </MaterialUi.List>,
  };
};
