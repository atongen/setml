open Belt;

type canvas;

[@bs.send] external getContext : (canvas, string) => Canvas2dRe.t = "";

type action =
  | Click((int, int));

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
  Canvas2dRe.setFillStyle(ctx, Canvas2dRe.String, color);
  Canvas2dRe.fillRect(~x=Util.round(x), ~y=Util.round(y), ~w=Util.round(w), ~h=Util.round(h), ctx);
};

let reset = (ctx, color, width, height) => {
  Canvas2dRe.setFillStyle(ctx, Canvas2dRe.String, color);
  Canvas2dRe.fillRect(~x=0.0, ~y=0.0, ~w=float_of_int(width), ~h=float_of_int(height), ctx);
  ();
};

let blockSize = (width, height, columns, rows) => {
    let idealWidth = width /. columns;
    let idealHeight = height /. rows;
    if (idealWidth >= idealHeight) {
        let xOffset = (width -. idealHeight *. columns) /. 2.;
        (idealHeight, xOffset, 0.);
    } else {
        let yOffset = (height -. idealWidth *. rows) /. 2.;
        (idealWidth, 0., yOffset);
    };
};

let border = (width, height) => {
    min(width, height) /. 25.;
};

let drawBorder = (ctx, size, width, height, xOffset, yOffset) => {
    let x = xOffset +. (size /. 2.);
    let y = yOffset +. (size /. 2.);
    let w = width -. 2. *. x;
    let h = height -. 2. *. y;
    drawBlock(ctx, randomColor(), x, y, w, h);
};

let drawBlocks = (ctx, width, height, columns, rows) => {
  let (w, h, c, r) = Util.map_quad(float_of_int, (width, height, columns, rows));
  let b = border(w, h);
  let (bs, xOffset, yOffset) = blockSize(w -. 2. *. b, h -. 2. *. b, c, r);
  drawBorder(ctx, b, w, h, xOffset, yOffset);
  for (i in 0 to columns - 1) {
    for (j in 0 to rows - 1) {
      let x = float_of_int(i) *. bs;
      let y = float_of_int(j) *. bs;
      drawBlock(ctx, randomColor(), x +. xOffset +. b, y +. yOffset +. b, bs, bs);
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
    /*Canvas2dRe.scale(ratio, ratio, context);*/
    let width = right - left;
    let height = bottom - top;
    reset(context, "black", width, height);
    drawBlocks(context, width, height, columns, rows);
    ReasonReact.NoUpdate;
  },
  didUpdate: ({oldSelf, newSelf}) =>
    switch (newSelf.state.context) {
    | {contents: Some(ctx)} =>
      let width = right - left;
      let height = bottom - top;
      reset(ctx, "black", width, height);
      drawBlocks(ctx, width, height, columns, rows);
    | _ => Js.log("Unable to redraw blocks: No context found!")
    },
  render: ({state, send}) => {
    let width = right - left;
    let height = bottom - top;
    <canvas
      id="board"
      width=(string_of_int(width))
      height=(string_of_int(height))
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
