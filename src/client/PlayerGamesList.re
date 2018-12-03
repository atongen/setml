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
    |> then_(json =>
         json |> player_games_of_json |> (player_games => Some(player_games) |> resolve)
       )
    |> catch(_err => resolve(None))
  );

type action =
  | LoadPlayerGames
  | LoadedPlayerGames(list(player_game))
  | LoadPlayerGamesFailed;

let component = ReasonReact.reducerComponent("PlayerGamesList");

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
  render: self =>
    switch (self.state) {
    | NotAsked =>
        switch(ClientUtil.current_player_id()) {
        | Some(_player_id) => self.send(LoadPlayerGames)
        | None => ()
        };
        ReasonReact.null
    | Loading => <div> (ReasonReact.string("Loading...")) </div>
    | Failure => <div> (ReasonReact.string("Something went wrong!")) </div>
    | Success(player_games) =>
        if (List.length(player_games) > 0) {
            <div>
                <h2> (ReasonReact.string("Games")) </h2>
                <ul>
                (
                    player_games
                    |> List.map(player_game => <li key=(string_of_int(player_game.id))> (ReasonReact.string(player_game_to_string(player_game))) </li>)
                    |> Array.of_list
                    |> ReasonReact.array
                )
                </ul>
            </div>
        } else {
            ReasonReact.null
        }
    },
};
