open Belt;

type canvas;

[@bs.send] external getContext : (canvas, string) => Canvas2dRe.t = "";

type action =
  | Click((int, int))
  | Hover((int, int));

type rect = {
  x: float,
  y: float,
  w: float,
  h: float,
};

type dimensions = {
  top: int,
  bottom: int,
  left: int,
  right: int,
  ratio: float,
  columns: int,
  rows: int,
  width: float,
  height: float,
  border: rect,
  blocks: array(rect),
};

type state = {
  dims: dimensions,
  hovered: option(int),
  selected: Set.Int.t,
  context: ref(option(Canvas2dRe.t)),
};

let component = ReasonReact.reducerComponent("Board");

let getClick = evt => Click((ReactEventRe.Mouse.clientX(evt), ReactEventRe.Mouse.clientY(evt)));

let getHover = evt => Hover((ReactEventRe.Mouse.clientX(evt), ReactEventRe.Mouse.clientY(evt)));

let isWithin = (rect, (x, y)) => rect.x <= x && x <= rect.x +. rect.w && rect.y <= y && y <= rect.y +. rect.h;

let inBlock = (blocks, (x, y)) => {
  let rec aux = i =>
    switch (blocks[i]) {
    | Some(block) =>
      if (isWithin(block, (x, y))) {
        Some(i);
      } else {
        aux(i + 1);
      }
    | None => None
    };
  aux(0);
};

let randomColor = () => {
  let r = Random.int(256);
  let g = Random.int(256);
  let b = Random.int(256);
  "rgb(" ++ string_of_int(r) ++ ", " ++ string_of_int(g) ++ ", " ++ string_of_int(b) ++ ")";
};

let drawRectangle = (ctx, color, x, y, w, h) => {
  Canvas2dRe.setFillStyle(ctx, Canvas2dRe.String, color);
  Canvas2dRe.fillRect(~x=Util.round(x), ~y=Util.round(y), ~w=Util.round(w), ~h=Util.round(h), ctx);
};

let drawRect = (ctx, color, rect) => drawRectangle(ctx, color, rect.x, rect.y, rect.w, rect.h);

let drawBlock = (ctx, color, idx, rect) => {
  drawRect(ctx, color, rect);
  Canvas2dRe.font(ctx, "24px serif");
  Canvas2dRe.strokeText(string_of_int(idx), ctx, ~x=rect.x +. 30., ~y=rect.y +. 30.);
};

let reset = (ctx, color, width, height) => drawRectangle(ctx, color, 0.0, 0.0, width, height);

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

let border = (width, height) => min(width, height) /. 25.;

let drawBoard = (ctx, dims) => {
  drawRect(ctx, randomColor(), dims.border);
  for (i in 0 to dims.rows - 1) {
    for (j in 0 to dims.columns - 1) {
      let idx = i * dims.columns + j;
      switch (dims.blocks[idx]) {
      | Some(rect) => drawBlock(ctx, randomColor(), idx, rect)
      | None => Js.log("drawBoard error: No block found at idx " ++ string_of_int(idx) ++ "!")
      };
    };
  };
};

let makeDims = (top, bottom, left, right, ratio, columns, rows) => {
  let w = float_of_int(right - left);
  let h = float_of_int(bottom - top);
  let c = float_of_int(columns);
  let r = float_of_int(rows);
  let b = border(w, h);
  let (bs, xOffset, yOffset) = blockSize(w -. 2. *. b, h -. 2. *. b, c, r);
  let borderX = xOffset +. b /. 2.;
  let borderY = yOffset +. b /. 2.;
  let emptyRect = {x: 0., y: 0., w: 0., h: 0.};
  let blocks = Array.make(columns * rows, emptyRect);
  for (i in 0 to rows - 1) {
    for (j in 0 to columns - 1) {
      let bx = float_of_int(j) *. bs;
      let by = float_of_int(i) *. bs;
      let bRect = {x: bx +. xOffset +. b, y: by +. yOffset +. b, w: bs, h: bs};
      let updated = blocks[i * columns + j] = bRect;
      assert updated;
    };
  };
  {
    top,
    bottom,
    left,
    right,
    ratio,
    columns,
    rows,
    width: w,
    height: h,
    border: {
      x: borderX,
      y: borderY,
      w: w -. 2. *. borderX,
      h: h -. 2. *. borderY,
    },
    blocks,
  };
};

let shouldRedraw = (oldDims: dimensions, newDims: dimensions) =>
  oldDims.top != newDims.top
  || oldDims.bottom != newDims.bottom
  || oldDims.left != newDims.left
  || oldDims.right != newDims.right;

let make = (_children, ~top, ~bottom, ~left, ~right, ~ratio, ~columns, ~rows) => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | Click((x, y)) =>
      switch (inBlock(state.dims.blocks, (float_of_int(x), float_of_int(y)))) {
      | Some(idx) =>
        let f = Set.Int.(has(state.selected, idx) ? remove : add);
        let newSelected = f(state.selected, idx);
        Js.log(
          "Current selections: (" ++ String.concat(",", List.map(Set.Int.toList(newSelected), string_of_int)) ++ ")",
        );
        ReasonReact.Update({...state, selected: newSelected});
      | None => ReasonReact.NoUpdate
      }
    | Hover((x, y)) =>
      let newHovered = inBlock(state.dims.blocks, (float_of_int(x), float_of_int(y)));
      switch (state.hovered, newHovered) {
      | (Some(oldIdx), Some(newIdx)) =>
        if (oldIdx == newIdx) {
          ReasonReact.NoUpdate;
        } else {
          /*Js.log("Started hovering " ++ string_of_int(newIdx) ++ ", stopped hovering " ++ string_of_int(oldIdx));*/
          ReasonReact.Update({
            ...state,
            hovered: Some(newIdx),
          });
        }
      | (Some(oldIdx), None) =>
        /*Js.log("Stopped hovering " ++ string_of_int(oldIdx));*/
        ReasonReact.Update({...state, hovered: None})
      | (None, Some(newIdx)) =>
        /*Js.log("Started hovering " ++ string_of_int(newIdx));*/
        ReasonReact.Update({...state, hovered: Some(newIdx)})
      | (None, None) => ReasonReact.NoUpdate
      };
    },
  initialState: () => {
    dims: makeDims(top, bottom, left, right, ratio, columns, rows),
    hovered: None,
    selected: Set.Int.empty,
    context: ref(None),
  },
  willReceiveProps: self => {...self.state, dims: makeDims(top, bottom, left, right, ratio, columns, rows)},
  didMount: self => {
    let myCanvas: canvas = [%bs.raw {| document.getElementById("board") |}];
    let context = getContext(myCanvas, "2d");
    self.state.context := Some(context);
    reset(context, "white", self.state.dims.width, self.state.dims.height);
    drawBoard(context, self.state.dims);
    let deck = Game_deck.make(12, 45);
    Js.log("Length: " ++ string_of_int(Array.length(deck)));
    Array.forEach(deck, c => Js.log(string_of_int(Card.to_int(c)) ++ ": " ++ Card.to_string(c)));
    ReasonReact.NoUpdate;
  },
  didUpdate: ({oldSelf, newSelf}) =>
    if (shouldRedraw(oldSelf.state.dims, newSelf.state.dims)) {
      switch (newSelf.state.context) {
      | {contents: Some(ctx)} =>
        reset(ctx, "white", newSelf.state.dims.width, newSelf.state.dims.height);
        drawBoard(ctx, newSelf.state.dims);
      | _ => Js.log("Unable to redraw blocks: No context found!")
      };
    },
  render: ({state, send}) =>
    <canvas
      id="board"
      width=(string_of_float(state.dims.width))
      height=(string_of_float(state.dims.height))
      style=(
        ReactDOMRe.Style.make(
          ~top=string_of_int(state.dims.top) ++ "px",
          ~bottom=string_of_int(state.dims.bottom) ++ "px",
          ~left=string_of_int(state.dims.left) ++ "px",
          ~right=string_of_int(state.dims.right) ++ "px",
          ~width=string_of_float(state.dims.width) ++ "px",
          ~height=string_of_float(state.dims.height) ++ "px",
          (),
        )
      )
      onClick=(evt => send(getClick(evt)))
      onMouseMove=(evt => send(getHover(evt)))
    />,
};
