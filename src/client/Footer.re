let component = ReasonReact.statelessComponent("Footer");

let footerStyle = ReactDOMRe.Style.make(~marginTop="24px", ~textAlign="center", ());

let make = _children => {
  ...component,
  render: _self =>
    MaterialUi.(
      <footer style=footerStyle>
        <Typography>
          <a target="_blank" href="https://www.setgame.com/"> (ReasonReact.string("Set Game")) </a>
          (ReasonReact.string(" | "))
          <a target="_blank" href="https://github.com/atongen/setml"> (ReasonReact.string("Source Code")) </a>
        </Typography>
      </footer>
    ),
};
