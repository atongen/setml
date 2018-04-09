let component = ReasonReact.statelessComponent("Sidebar");

let make = (_children, ~top, ~bottom, ~left, ~right, ~summary) => {
  ...component,
  render: _self => {
    Js.log(
      "rendering Sidebar - top: "
      ++ string_of_int(top)
      ++ ", bottom: "
      ++ string_of_int(bottom)
      ++ ", left: "
      ++ string_of_int(left)
      ++ ", right: "
      ++ string_of_int(right)
      ++ ", width: "
      ++ string_of_int(right - left)
      ++ ", height: "
      ++ string_of_int(bottom - top)
      ++ ", summary: "
      ++ string_of_bool(summary),
    );
    <div
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
      <p> (ReasonReact.stringToElement("Hello!")) </p>
    </div>;
  },
};
