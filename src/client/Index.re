module NewGameForm = {
  let component = ReasonReact.statelessComponent("NewGameForm");
  let make = (~token, _children) => {
    ...component,
    render: _ =>
      <section>
        <MaterialUi.Typography variant=`H1> (ReasonReact.string("SetML")) </MaterialUi.Typography>
        <form action="/games" method="POST" encType="application/x-www-form-urlencoded">
          <input hidden=true name="token" value=token readOnly=true />
          <div>
            MaterialUi.(
              <Button variant=`Contained color=`Primary type_="submit">
                (ReasonReact.string("New Game"))
                <Icon> (ReasonReact.string("group_add")) </Icon>
              </Button>
            )
          </div>
        </form>
        <PlayerGamesList />
      </section>,
  };
};

let token = ClientUtil.meta_content("token");

ReactDOMRe.renderToElementWithId(<NewGameForm token />, "index");
