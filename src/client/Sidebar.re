let component = ReasonReact.statelessComponent("Sidebar");

let game_url =
  switch (Util.game_url()) {
  | Some(url) => url
  | None => ""
  };

let make = (_children, ~top, ~bottom, ~left, ~right, ~summary) => {
  ...component,
  render: _self =>
    <section
      id="sidebar"
      style=(
        ReactDOMRe.Style.make(
          ~top=string_of_int(top) ++ "px",
          ~bottom=string_of_int(bottom) ++ "px",
          ~left=string_of_int(left) ++ "px",
          ~right=string_of_int(right) ++ "px",
          ~width=string_of_int(right - left) ++ "px",
          ~height=string_of_int(bottom - top) ++ "px",
          (),
        )
      )>
      <div id="header">
        <h1> <a href="/"> (ReasonReact.stringToElement("SetML")) </a> </h1>
        <ul>
          <li>
            (ReasonReact.stringToElement("Game: "))
            <a href=game_url> (ReasonReact.stringToElement(game_url)) </a>
          </li>
          <li> (ReasonReact.stringToElement("Sets on board: 4")) </li>
          <li> (ReasonReact.stringToElement("Cards remaining: 81")) </li>
        </ul>
        <button> (ReasonReact.stringToElement("Start!")) </button>
      </div>
      <div id="scores">
        <ul>
          <li> (ReasonReact.stringToElement("Alice: 1")) </li>
          <li> (ReasonReact.stringToElement("Bob: 2")) </li>
          <li> (ReasonReact.stringToElement("Carol: 3")) </li>
        </ul>
      </div>
    </section>,
};
