module NewGameForm = {
  let component = ReasonReact.statelessComponent("NewGameForm");
  let make = (~token, _children) => {
    ...component,
    render: _ =>
      <section>
        <h1> (ReasonReact.string("SetML")) </h1>
        <form action="/games" method="POST" encType="application/x-www-form-urlencoded">
          <input hidden=true name="token" value=token readOnly=true />
          <div>
            <button>
              (ReasonReact.string("Start a new game!"))
            </button>
          </div>
        </form>
      </section>,
  };
};

let token = ClientUtil.meta_content("token");

ReactDOMRe.renderToElementWithId(<NewGameForm token />, "index");
