open Belt;

type canvas;

[@bs.send] external getContext : (canvas, string) => Canvas2dRe.t = "";

type action =
  | Click((int, int))
  | Resize;

type state = {
  num: int,
  context: ref(option(Canvas2dRe.t)),
};

let component = ReasonReact.reducerComponent("Board");

let getClick = evt => Click((ReactEventRe.Mouse.clientX(evt), ReactEventRe.Mouse.clientY(evt)));

let randomColor = () => {
  let r = Random.int(256);
  let g = Random.int(256);
  let b = Random.int(256);
  "rgb(" ++ string_of_int(r) ++ ", " ++ string_of_int(g) ++ ", " ++ string_of_int(b) ++ ")";
};

let drawBlock = (ctx, color, x, y, w, h) => {
  Js.log(
    "Drawing block - color: "
    ++ color
    ++ ", x: "
    ++ string_of_float(x)
    ++ ", y: "
    ++ string_of_float(y)
    ++ ", w: "
    ++ string_of_float(w)
    ++ ", h: "
    ++ string_of_float(h),
  );
  Canvas2dRe.setFillStyle(ctx, Canvas2dRe.String, color);
  Canvas2dRe.fillRect(~x=Util.round(x), ~y=Util.round(y), ~w=Util.round(w), ~h=Util.round(h), ctx);
};

let reset = (ctx, color, width, height) => {
  Canvas2dRe.setFillStyle(ctx, Canvas2dRe.String, color);
  Canvas2dRe.fillRect(~x=0.0, ~y=0.0, ~w=float_of_int(width), ~h=float_of_int(height), ctx);
  ();
};

let drawBlocks = (ctx, width, height, columns, rows) => {
  let blockWidth = float_of_int(width) /. float_of_int(columns);
  let blockHeight = float_of_int(height) /. float_of_int(rows);
  for (i in 0 to columns - 1) {
    for (j in 0 to rows - 1) {
      let x = float_of_int(i) *. blockWidth;
      let y = float_of_int(j) *. blockHeight;
      drawBlock(ctx, randomColor(), x, y, blockWidth, blockHeight);
    };
  };
};

let make = (_children, ~top, ~bottom, ~left, ~right, ~ratio, ~columns, ~rows) => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | Click((x, y)) =>
      let newNum = state.num + 1;
      Js.log(
        "Clicked " ++ string_of_int(newNum) ++ " times! (" ++ string_of_int(x) ++ "," ++ string_of_int(y) ++ ")",
      );
      ReasonReact.Update({...state, num: newNum});
    },
  initialState: () => {num: 0, context: ref(None)},
  didMount: self => {
    let myCanvas: canvas = [%bs.raw {| document.getElementById("board") |}];
    let context = getContext(myCanvas, "2d");
    self.state.context := Some(context);
    Canvas2dRe.scale(ratio, ratio, context);
    let width = right - left;
    let height = bottom - top;
    reset(context, "black", width, height);
    drawBlocks(context, width, height, columns, rows);
    ReasonReact.NoUpdate;
  },
  didUpdate: ({oldSelf, newSelf}) => {
    switch(newSelf.state.context) {
    | {contents: Some(ctx)} =>
        let width = right - left;
        let height = bottom - top;
        reset(ctx, "black", width, height);
        drawBlocks(ctx, width, height, columns, rows);
    | _ => Js.log("Unable to redraw blocks: No context found!");
    };
  },
  render: ({state, send}) => {
    Js.log("Board - rendering!");
    let width = right - left;
    let height = bottom - top;
    Js.log(
      "rendering Board - top: "
      ++ string_of_int(top)
      ++ ", bottom: "
      ++ string_of_int(bottom)
      ++ ", left: "
      ++ string_of_int(left)
      ++ ", right: "
      ++ string_of_int(right)
      ++ ", ratio: "
      ++ string_of_float(ratio)
      ++ ", width: "
      ++ string_of_int(width)
      ++ ", height: "
      ++ string_of_int(height),
    );
    <canvas
      id="board"
      width=string_of_int(width)
      height=string_of_int(height)
      style=(
        ReactDOMRe.Style.make(
          ~top=string_of_int(top) ++ "px",
          ~bottom=string_of_int(bottom) ++ "px",
          ~left=string_of_int(left) ++ "px",
          ~right=string_of_int(right) ++ "px",
          ~width=string_of_int(width) ++ "px",
          ~height=string_of_int(height) ++ "px",
          (),
        )
      )
      onClick=(evt => send(getClick(evt)))
    />;
  },
};
