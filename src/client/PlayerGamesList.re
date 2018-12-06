open Api_messages;

open ClientApiMessages;

open ClientApiMessageConverter;

let url = "/player_games";

type state =
  | NotAsked
  | Loading
  | Failure
  | Success(list(player_game));

let fetchPlayerGames = () =>
  Js.Promise.(
    Fetch.fetch(url)
    |> then_(Fetch.Response.text)
    |> then_(json => json |> player_games_of_json |> (player_games => Some(player_games) |> resolve))
    |> catch(_err => resolve(None))
  );

type action =
  | LoadPlayerGames
  | LoadedPlayerGames(list(player_game))
  | LoadPlayerGamesFailed;

let component = ReasonReact.reducerComponent("PlayerGamesList");

let renderPlayerGameLi = (player_game, now) => {
  let game_id_str = Base_conv.base36_of_int(player_game.id);
  let dtw = ClientUtil.distanceOfTimeInWords(now, player_game.updated_at);
  let icon = switch(player_game.status) {
  | New => "add"
  | Started => "forward"
  | Complete => "done"
  };
  let s = ReasonReact.string;
  let link = <a href=("/games/" ++ game_id_str)> (s(game_id_str)) </a>;
  MaterialUi.(
  <ListItem key=string_of_int(player_game.id)>
    <Avatar>
      <Icon> (s(icon)) </Icon>
    </Avatar>
    <ListItemText primary=link secondary=s(dtw) />
  </ListItem>
  )
};

let make = _children => {
  ...component,
  initialState: () => NotAsked,
  reducer: (action, _state) =>
    switch (action) {
    | LoadPlayerGames =>
      ReasonReact.UpdateWithSideEffects(
        Loading,
        (
          self =>
            Js.Promise.(
              fetchPlayerGames()
              |> then_(result =>
                   switch (result) {
                   | Some(player_games) => resolve(self.send(LoadedPlayerGames(player_games)))
                   | None => resolve(self.send(LoadPlayerGamesFailed))
                   }
                 )
              |> ignore
            )
        ),
      )
    | LoadedPlayerGames(player_games) => ReasonReact.Update(Success(player_games))
    | LoadPlayerGamesFailed => ReasonReact.Update(Failure)
    },
  didMount: self =>
    switch (ClientUtil.current_player_id()) {
    | Some(_player_id) => self.send(LoadPlayerGames)
    | None => ()
    },
  render: self =>
    switch (self.state) {
    | NotAsked => ReasonReact.null
    | Loading => <div> (ReasonReact.string("Loading player games...")) </div>
    | Failure => <div> (ReasonReact.string("Something went wrong!")) </div>
    | Success(player_games) =>
      if (List.length(player_games) > 0) {
        let now = Js.Date.now() /. 1000.0;
        <div>
          <MaterialUi.Typography variant=`H2> (ReasonReact.string("Recent Games")) </MaterialUi.Typography>
          <MaterialUi.List>
            (
              player_games
              |> List.map(player_game => (renderPlayerGameLi(player_game, now)))
              |> Array.of_list
              |> ReasonReact.array
            )
          </MaterialUi.List>
        </div>;
      } else {
        ReasonReact.null;
      }
    },
};
