open Belt;

open Messages;

type canvas;

[@bs.send] external getContext : (canvas, string) => Canvas2dRe.t = "";

type action =
  | Click((float, float))
  | Hover((float, float));

type dimensions = {
  ratio: float,
  columns: int,
  rows: int,
  size: Rect.t,
  border: Rect.t,
  blocks: array(Rect.t),
};

type state = {
  dims: dimensions,
  hovered: option(int),
  selected: Set.Int.t,
  context: ref(option(Canvas2dRe.t)),
  boardCards: array(option(Card.t)),
};

let component = ReasonReact.reducerComponent("Board");

let getClick = evt =>
  Click((float_of_int(ReactEventRe.Mouse.clientX(evt)), float_of_int(ReactEventRe.Mouse.clientY(evt))));

let getHover = evt =>
  Hover((float_of_int(ReactEventRe.Mouse.clientX(evt)), float_of_int(ReactEventRe.Mouse.clientY(evt))));

let randomColor = () => {
  let r = Random.int(256);
  let g = Random.int(256);
  let b = Random.int(256);
  "rgb(" ++ string_of_int(r) ++ ", " ++ string_of_int(g) ++ ", " ++ string_of_int(b) ++ ")";
};

let drawRectangle = (ctx, color, x, y, w, h) => {
  Canvas2dRe.setFillStyle(ctx, Canvas2dRe.String, color);
  Canvas2dRe.fillRect(
    ~x=Shared_util.round(x),
    ~y=Shared_util.round(y),
    ~w=Shared_util.round(w),
    ~h=Shared_util.round(h),
    ctx,
  );
};

let drawRect = (ctx, color, rect) => drawRectangle(ctx, color, rect.Rect.x, rect.y, rect.w, rect.h);

let drawBlock = (ctx, color, idx, rect, cardOpt) => {
  drawRect(ctx, color, rect);
  Canvas2dRe.font(ctx, "24px serif");
  let text = Printf.sprintf("%d (%d)", idx, Card.to_int_opt(cardOpt));
  Canvas2dRe.strokeText(text, ctx, ~x=rect.x +. 30., ~y=rect.y +. 30.);
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

let drawBoard = (ctx, dims, cards: array(option(Card.t))) => {
  drawRect(ctx, randomColor(), dims.border);
  for (i in 0 to dims.rows - 1) {
    for (j in 0 to dims.columns - 1) {
      let idx = i * dims.columns + j;
      switch (dims.blocks[idx], Array.get(cards, idx)) {
      | (Some(rect), Some(cardOpt)) => drawBlock(ctx, randomColor(), idx, rect, cardOpt)
      | (Some(rect), None) =>
        Js.log("drawBoard error: No board card provided at idx" ++ string_of_int(idx) ++ "!");
        drawBlock(ctx, randomColor(), idx, rect, None);
      | (None, _) => Js.log("drawBoard error: No block found at idx " ++ string_of_int(idx) ++ "!")
      };
    };
  };
};

let makeDims = (rect, ratio, columns, rows) => {
  let c = float_of_int(columns);
  let r = float_of_int(rows);
  let b = border(rect.Rect.w, rect.h);
  let (bs, xOffset, yOffset) = blockSize(rect.w -. 2. *. b, rect.h -. 2. *. b, c, r);
  let borderX = xOffset +. b /. 2.;
  let borderY = yOffset +. b /. 2.;
  let blocks = Array.make(columns * rows, Rect.empty);
  for (i in 0 to rows - 1) {
    for (j in 0 to columns - 1) {
      let bx = float_of_int(j) *. bs;
      let by = float_of_int(i) *. bs;
      let bRect = Rect.make(bx +. xOffset +. b, by +. yOffset +. b, bs, bs);
      let updated = blocks[i * columns + j] = bRect;
      assert updated;
    };
  };
  {
    ratio,
    columns,
    rows,
    size: rect,
    border: Rect.make(borderX, borderY, rect.w -. 2. *. borderX, rect.h -. 2. *. borderY),
    blocks,
  };
};

let printBoardCards = boardCards => {
    Array.forEachWithIndex(boardCards, (i, c) =>
        switch(c) {
        | Some(card) => Js.log(string_of_int(i) ++ ": " ++ Card.to_string(card));
        | None =>  Js.log(string_of_int(i) ++ ": NONE");
        }
    );
};

let printSets = boardCards =>
    switch(Card.next_set_and_indexes_of_opt_array(boardCards)) {
    | Some(((idx0, _), (idx1, _), (idx2, _))) => Js.log(Printf.sprintf("Set: (%d, %d, %d)", idx0, idx1, idx2));
    | None => Js.log("No sets on board!");
    };

let shouldRedraw = (oldState: state, newState: state) => {
  (oldState.dims.size != newState.dims.size) || (oldState.boardCards != newState.boardCards);
};

let isSet = (boardCards, idxs) =>
    if (Array.length(boardCards) != 12 || Set.Int.size(idxs) != 3) {
        false
    } else {
        let idxa = Set.Int.toArray(idxs);
        let c0o = Array.getExn(boardCards, Array.getExn(idxa, 0));
        let c1o = Array.getExn(boardCards, Array.getExn(idxa, 1));
        let c2o = Array.getExn(boardCards, Array.getExn(idxa, 2));
        switch (c0o, c1o, c2o) {
        | (Some(c0), Some(c1), Some(c2)) => Card.is_set(c0, c1, c2);
        | (_, _, _) => false;
        }
    };

let make = (_children, ~rect, ~ratio, ~columns, ~rows, ~boardCards) => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | Click((x, y)) =>
      switch (Rect.findRect(state.dims.blocks, (x, y))) {
      | Some(idx) =>
        if (Set.Int.has(state.selected, idx)) {
            let newSelected = Set.Int.remove(state.selected, idx);
            Js.log("Current selections: (" ++ String.concat(",", List.map(Set.Int.toList(newSelected), string_of_int)) ++ ")");
            ReasonReact.Update({...state, selected: newSelected});
        } else {
            let newSelected = Set.Int.add(state.selected, idx);
            let l = Set.Int.size(newSelected);
            if (l < 3) {
                Js.log("Current selections: (" ++ String.concat(",", List.map(Set.Int.toList(newSelected), string_of_int)) ++ ")");
                ReasonReact.Update({...state, selected: newSelected});
            } else {
                if (isSet(state.boardCards, newSelected)) {
                    Js.log("You got a set!");
                } else {
                    Js.log("That's not a set, dummy!");
                };
                Js.log("Current selections: ()");
                ReasonReact.Update({...state, selected: Set.Int.empty});
            };
        }
      | None => ReasonReact.NoUpdate
      }
    | Hover((x, y)) =>
      let newHovered = Rect.findRect(state.dims.blocks, (x, y));
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
    dims: makeDims(rect, ratio, columns, rows),
    hovered: None,
    selected: Set.Int.empty,
    context: ref(None),
    boardCards,
  },
  willReceiveProps: self => {...self.state, dims: makeDims(rect, ratio, columns, rows), boardCards},
  didMount: self => {
    let myCanvas: canvas = [%bs.raw {| document.getElementById("board") |}];
    let context = getContext(myCanvas, "2d");
    self.state.context := Some(context);
    reset(context, "white", self.state.dims.size.w, self.state.dims.size.h);
    drawBoard(context, self.state.dims, self.state.boardCards);
    ReasonReact.NoUpdate;
  },
  didUpdate: ({oldSelf, newSelf}) =>
    if (shouldRedraw(oldSelf.state, newSelf.state)) {
      printSets(newSelf.state.boardCards);
      switch (newSelf.state.context) {
      | {contents: Some(ctx)} =>
        reset(ctx, "white", newSelf.state.dims.size.w, newSelf.state.dims.size.h);
        drawBoard(ctx, newSelf.state.dims, newSelf.state.boardCards);
      | _ => Js.log("Unable to redraw blocks: No context found!")
      };
    },
  render: ({state, send}) => {
    <canvas
      id="board"
      width=(Shared_util.roundis(state.dims.size.w))
      height=(Shared_util.roundis(state.dims.size.h))
      style=(Rect.toStyle(rect))
      onClick=(evt => send(getClick(evt)))
      onMouseMove=(evt => send(getHover(evt)))
    />
  }
};
