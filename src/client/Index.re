module NewGameForm = {
  let formStyle = ReactDOMRe.Style.make(~width="100%", ());
  let component = ReasonReact.statelessComponent("NewGameForm");
  let make = (~token, _children) => {
    ...component,
    render: _self =>
      MaterialUi.(
        <div>
          <form style=formStyle action="/games" method="POST" encType="application/x-www-form-urlencoded">
            <input hidden=true name="token" value=token readOnly=true />
            <div>
              <Button variant=`Contained color=`Primary type_="submit" fullWidth=true>
                (ReasonReact.string("New Game"))
                <Icon> (ReasonReact.string("group_add")) </Icon>
              </Button>
            </div>
          </form>
          <Typography>
            (ReasonReact.string("Once you've created a new game, you can share the URL to invite other players."))
          </Typography>
        </div>
      ),
  };
};

let maybeToken = ClientUtil.meta_content_opt("token");
let form =
  switch (maybeToken) {
  | Some(token) => <NewGameForm token />
  | None => ReasonReact.null
  };

let mainStyle = ReactDOMRe.Style.make(~width="400px", ~display="block", ~marginLeft="auto", ~marginRight="auto", ());
let paperStyle =
  ReactDOMRe.Style.make(
    ~display="flex",
    ~flexDirection="column",
    ~alignItems="center",
    ~marginTop="24px",
    ~padding="6px 9px 9px",
    (),
  );

let content =
  <main style=mainStyle>
    MaterialUi.(
      <CssBaseline>
        <Paper style=paperStyle>
          <Typography variant=`H1> (ReasonReact.string("SetML")) </Typography>
          <Typography variant=`Subtitle1>
            (ReasonReact.string("Online, realtime, multiplayer game of Set"))
          </Typography>
          form
          <PlayerGamesList />
        </Paper>
        <Footer />
      </CssBaseline>
    )
  </main>;

ReactDOMRe.renderToElementWithId(content, "index");
