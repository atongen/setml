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

let blockSize = (size, num, border) => {
  let s = float_of_int(size);
  let n = float_of_int(num);
  (s -. border *. (2. +. 2. *. n)) /. n;
};

let round = v => floor(v +. 0.5);

let drawBlock = (ctx, color, x, y, s) => {
  let v = round(s);
  Js.log("x: " ++ string_of_float(x) ++ ", y: " ++ string_of_float(y) ++ ", s: " ++ string_of_float(v));
  Canvas2dRe.setFillStyle(ctx, Canvas2dRe.String, color);
  Canvas2dRe.fillRect(~x=round(x), ~y=round(y), ~h=v, ~w=v, ctx);
};

let getCorner = (size, border, i, j) => {
  let aux = (size, border, n) => 2. *. border +. 2. *. n *. border +. size *. n;
  (aux(size, border, float_of_int(i)), aux(size, border, float_of_int(j)));
};

let drawBlocks = (ctx, color, width, height, columns, rows) => {
  let borderRatio = 50.;
  let (border, bs) =
    if (width >= height) {
      /* landscape */
      let border = round(float_of_int(height) /. borderRatio);
      let bs = blockSize(height, rows, border);
      (border, bs);
    } else {
      /* portrait */
      let border = round(float_of_int(width) /. borderRatio);
      let bs = blockSize(width, columns, border);
      (border, bs);
    };
  Js.log("Block size: " ++ string_of_float(bs));
  for (i in 0 to columns - 1) {
    for (j in 0 to rows - 1) {
      Js.log("drawing i: " ++ string_of_int(i) ++ ", j: " ++ string_of_int(j));
      let (x, y) = getCorner(bs, border, i, j);
      Js.log("drawing x: " ++ string_of_float(x) ++ ", y: " ++ string_of_float(y));
      drawBlock(ctx, color, x, y, bs);
    };
  };
};

let make = (_children, ~name, ~width, ~height) => {
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
    let myCanvas: canvas = [%bs.raw {| document.getElementById("myCanvas") |}];
    let context = getContext(myCanvas, "2d");
    self.state.context := Some(context);
    drawBlocks(context, "red", width, height, 4, 3);
    ReasonReact.NoUpdate;
  },
  render: ({state, send}) => {
    Js.log("width: " ++ string_of_int(width) ++ ", height: " ++ string_of_int(height));
    <canvas
      onClick=(evt => send(getClick(evt)))
      id=name
      width=(string_of_int(width))
      height=(string_of_int(height))
    />;
  },
};
