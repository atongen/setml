open Belt;

let meta_content = name => {
  let rec aux = c =>
    switch (c) {
    | [] => ""
    | [hd, ...tl] =>
      let el_name = Webapi.Dom.Element.getAttribute("name", hd);
      switch (el_name) {
      | None => aux(tl)
      | Some(n) =>
        if (n == name) {
          let el_content = Webapi.Dom.Element.getAttribute("content", hd);
          switch (el_content) {
          | None => ""
          | Some(c) => c
          };
        } else {
          aux(tl);
        }
      };
    };
  DocumentRe.getElementsByTagName("meta", Webapi.Dom.document)
  |> Webapi.Dom.HtmlCollection.toArray
  |> Belt.List.fromArray
  |> aux;
};

module NewGameForm = {
  let component = ReasonReact.statelessComponent("NewGameForm");
  let make = (~token, _children) => {
    ...component,
    render: self =>
      <section>
        <h1> (ReasonReact.stringToElement("SetML")) </h1>
        <form action="/games" method="POST">
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

let token = meta_content("token");

ReactDOMRe.renderToElementWithId(<NewGameForm token />, "index");
