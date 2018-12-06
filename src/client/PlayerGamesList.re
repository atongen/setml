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

let renderPlayerGame = (player_game, now) => {
  let game_id_str = Base_conv.base36_of_int(player_game.id);
  let status = Game_status.to_string(player_game.status);
  let dtw = ClientUtil.distanceOfTimeInWords(now, player_game.updated_at);
  <div>
    <a href=("/games/" ++ game_id_str)> (ReasonReact.string(game_id_str)) </a>
    <span> (ReasonReact.string(Printf.sprintf("%s - %s", status, dtw))) </span>
  </div>;
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
          <h2> (ReasonReact.string("Recent Games")) </h2>
          <ul>
            (
              player_games
              |> List.map(player_game =>
                   <li key=(string_of_int(player_game.id))> (renderPlayerGame(player_game, now)) </li>
                 )
              |> Array.of_list
              |> ReasonReact.array
            )
          </ul>
        </div>;
      } else {
        ReasonReact.null;
      }
    },
};
