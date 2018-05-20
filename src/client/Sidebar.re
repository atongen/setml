let component = ReasonReact.statelessComponent("Sidebar");

let game_url =
  switch (ClientUtil.game_url()) {
  | Some(url) => url
  | None => ""
  };

let make = (_children, ~rect) => {
  ...component,
  render: _self =>
    <section id="sidebar" style=(Rect.toStyle(rect))>
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
        <h2> (ReasonReact.stringToElement("Score")) </h2>
        <ul>
          <li> (ReasonReact.stringToElement("Alice: 1")) </li>
          <li> (ReasonReact.stringToElement("Bob: 2")) </li>
          <li> (ReasonReact.stringToElement("Carol: 3")) </li>
        </ul>
      </div>
    </section>,
};
