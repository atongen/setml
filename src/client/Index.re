module NewGameForm = {
  let component = ReasonReact.statelessComponent("NewGameForm");
  let make = (~token, _children) => {
    ...component,
    render: _ =>
      <section>
        <h1> (ReasonReact.stringToElement("SetML")) </h1>
        <form action="/games" method="POST" encType="application/x-www-form-urlencoded">
          <input hidden=Js.true_ name="token" value=token readOnly=Js.true_ />
          <div>
            <button>
              (ReasonReact.stringToElement("Start a new game!"))
            </button>
          </div>
        </form>
      </section>,
  };
};

let token = ClientUtil.meta_content("token");

ReactDOMRe.renderToElementWithId(<NewGameForm token />, "index");
