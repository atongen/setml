let component = ReasonReact.statelessComponent("PlayAgainButton");

let make = (_children, ~maybeGameId) => {
  ...component,
  render: _self =>
    switch (maybeGameId) {
    | Some(gid) =>
      let gameId = Base_conv.base36_of_int(gid);
      MaterialUi.(
        <Button variant=`Contained color=`Primary href=("/games/" ++ gameId)>
          (ReasonReact.string("Play Again"))
          <Icon> (ReasonReact.string("group_add")) </Icon>
        </Button>
      );
    | None =>
      let maybeToken = ClientUtil.meta_content_opt("token");
      let maybeCurrentGameId = ClientUtil.game_id();
      switch (maybeToken, maybeCurrentGameId) {
      | (Some(token), Some(currentGameId)) =>
        <form action="/games" method="POST" encType="application/x-www-form-urlencoded">
          <input hidden=true name="token" value=token readOnly=true />
          <input hidden=true name="previous_game_id" value=currentGameId readOnly=true />
          <div>
            MaterialUi.(
              <Button variant=`Contained color=`Primary type_="submit">
                (ReasonReact.string("Play Again"))
                <Icon> (ReasonReact.string("replay")) </Icon>
              </Button>
            )
          </div>
        </form>
      | _o =>
        Js.log("Unable to get token or current game id");
        ReasonReact.null;
      };
    },
};
