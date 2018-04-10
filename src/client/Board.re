open Belt;

type canvas;

[@bs.send] external getContext : (canvas, string) => Canvas2dRe.t = "";

type action =
  | Click((int, int));

type state = {
  num: int,
  context: ref(option(Canvas2dRe.t)),
};

type rect = {
    x: float,
    y: float,
    w: float,
    h: float,
};

let isWithin = (rect, (x, y)) => {
    rect.x <= x && x <= rect.x +. rect.w &&
    rect.y <= y && y <= rect.y +. rect.h;
};

type props = {
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

let component = ReasonReact.reducerComponentWithRetainedProps("Board");

let getClick = evt => Click((ReactEventRe.Mouse.clientX(evt), ReactEventRe.Mouse.clientY(evt)));

let clickBlock = (blocks, (x, y)) => {
    let rec aux = (i) => {
        switch(Array.get(blocks, i)) {
        | Some(block) =>
            if (isWithin(block, (x, y))) {
                Some(i);
            } else {
                aux(i+1);
            }
        | None => None;
        };
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

let drawRect = (ctx, color, rect) => {
    drawRectangle(ctx, color, rect.x, rect.y, rect.w, rect.h);
};

let drawBlock = (ctx, color, idx, rect) => {
    drawRect(ctx, color, rect);
    Canvas2dRe.font(ctx, "24px serif");
    Canvas2dRe.strokeText(string_of_int(idx), ctx, ~x=(rect.x)+.30., ~y=(rect.y)+.30.);
};

let reset = (ctx, color, width, height) => {
  drawRectangle(ctx, color, 0.0, 0.0, width, height);
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

let drawBoard = (ctx, props) => {
    drawRect(ctx, randomColor(), props.border);
    for (i in 0 to props.rows - 1) {
        for (j in 0 to props.columns - 1) {
            let idx = i * props.columns + j;
            switch(Array.get(props.blocks, idx)) {
            | Some(rect) => drawBlock(ctx, randomColor(), idx, rect);
            | None => Js.log("drawBoard error: No block found at idx " ++ string_of_int(idx) ++ "!");
            }
        };
    };
};

let makeProps = (top, bottom, left, right, ratio, columns, rows) => {
    let w = float_of_int(right - left);
    let h = float_of_int(bottom - top);
    let c = float_of_int(columns);
    let r = float_of_int(rows);

    let b = border(w, h);
    let (bs, xOffset, yOffset) = blockSize(w -. 2. *. b, h -. 2. *. b, c, r);
    let borderX = xOffset +. (b /. 2.);
    let borderY = yOffset +. (b /. 2.);

    let emptyRect = {x:0.,y:0.,w:0.,h:0.};
    let blocks = Array.make(columns * rows, emptyRect);
    for (i in 0 to rows - 1) {
        for (j in 0 to columns - 1) {
            let bx = float_of_int(j) *. bs;
            let by = float_of_int(i) *. bs;
            let bRect = {
                x: bx +. xOffset +. b,
                y: by +. yOffset +. b,
                w: bs,
                h: bs,
            };
            let updated = Array.set(blocks, i * columns + j, bRect);
            assert(updated);
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
        blocks
    }
};


let shouldRedraw = (oldProps, newProps) => {
    oldProps.top != newProps.top ||
    oldProps.bottom != newProps.bottom ||
    oldProps.left != newProps.left ||
    oldProps.right != newProps.right;
};

let make = (_children, ~top, ~bottom, ~left, ~right, ~ratio, ~columns, ~rows) => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | Click((x, y)) =>
      /* Must have access to retained props here for clicks! */
      let newNum = state.num + 1;
      Js.log(
        "Clicked " ++ string_of_int(newNum) ++ " times! (" ++ string_of_int(x) ++ "," ++ string_of_int(y) ++ ")",
      );
      ReasonReact.Update({...state, num: newNum});
    },
  initialState: () => {num: 0, context: ref(None)},
  retainedProps: makeProps(top, bottom, left, right, ratio, columns, rows),
  didMount: self => {
    let myCanvas: canvas = [%bs.raw {| document.getElementById("board") |}];
    let context = getContext(myCanvas, "2d");
    self.state.context := Some(context);
    reset(context, "black", self.retainedProps.width, self.retainedProps.height);
    drawBoard(context, self.retainedProps);
    ReasonReact.NoUpdate;
  },
  didUpdate: ({oldSelf, newSelf}) => {
    if (shouldRedraw(oldSelf.retainedProps, newSelf.retainedProps)) {
        switch (newSelf.state.context) {
        | {contents: Some(ctx)} =>
            reset(ctx, "black", newSelf.retainedProps.width, newSelf.retainedProps.height);
            drawBoard(ctx, newSelf.retainedProps);
        | _ => Js.log("Unable to redraw blocks: No context found!")
        };
    }
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
