open Belt;

type canvas;

[@bs.send] external getContext : (canvas, string) => Canvas2dRe.t = "";

module BoardCardDataComparator =
  Belt.Id.MakeComparable(
    {
      type t = Messages.board_card_data;
      let cmp = (a: t, b: t) => Pervasives.compare(a, b);
    },
  );

let emptySelections = Set.make(~id=(module BoardCardDataComparator));

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
  selected: Set.t(Messages.board_card_data, BoardCardDataComparator.identity),
  context: ref(option(Canvas2dRe.t)),
  boardCards: list(Messages.board_card_data),
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

let getBoardCard = (boardCards: list(Messages.board_card_data), idx) =>
  switch (List.getBy(boardCards, bcd => bcd.idx == idx)) {
  | Some(bcd) => Some(bcd)
  | None => None
  };

let drawBoard = (ctx, dims, cards: list(Messages.board_card_data)) => {
  drawRect(ctx, randomColor(), dims.border);
  for (i in 0 to dims.rows - 1) {
    for (j in 0 to dims.columns - 1) {
      let idx = i * dims.columns + j;
      switch (dims.blocks[idx], getBoardCard(cards, idx)) {
      | (Some(rect), Some(bcd)) => drawBlock(ctx, randomColor(), idx, rect, bcd.card)
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

let printSets = (boardCards: list(Messages.board_card_data)) =>
  switch (Messages_util.board_cards_next_set(boardCards)) {
  | Some((c0, c1, c2)) =>
    let s = c => Messages.card_data_to_string(c);
    Js.log(Printf.sprintf("Set: (%s, %s, %s)", s(c0), s(c1), s(c2)));
  | None => Js.log("No sets on board!")
  };

let shouldRedraw = (oldState: state, newState: state) =>
  oldState.dims.size != newState.dims.size || oldState.boardCards != newState.boardCards;

let make = (_children, ~rect, ~ratio, ~columns, ~rows, ~boardCards, ~sendMessage) => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | Click((x, y)) =>
      switch (Rect.findRect(state.dims.blocks, (x, y))) {
      | Some(idx) =>
        switch (getBoardCard(state.boardCards, idx)) {
        | Some(bcd) =>
          if (Set.has(state.selected, bcd)) {
            let newSelected = Set.remove(state.selected, bcd);
            Js.log(
              "Current selections: ("
              ++ String.concat(",", List.map(Set.toList(newSelected), Messages.board_card_data_to_string))
              ++ ")",
            );
            ReasonReact.Update({...state, selected: newSelected});
          } else {
            let newSelected = Set.add(state.selected, bcd);
            let l = Set.size(newSelected);
            if (l < 3) {
              Js.log(
                "Current selections: ("
                ++ String.concat(",", List.map(Set.toList(newSelected), Messages.board_card_data_to_string))
                ++ ")",
              );
              ReasonReact.Update({...state, selected: newSelected});
            } else {
              let l = Set.toList(newSelected);
              switch (Messages_util.board_cards_list_is_set(l)) {
              | Some((cd0, cd1, cd2)) =>
                sendMessage(ClientUtil.make_move_msg(cd0, cd1, cd2));
                Js.log("You got a set!");
              | None => Js.log("That's not a set, dummy!")
              };
              Js.log("Current selections: ()");
              ReasonReact.Update({...state, selected: emptySelections});
            };
          }
        | None => ReasonReact.NoUpdate
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
    selected: emptySelections,
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
  render: ({state, send}) =>
    <canvas
      id="board"
      width=(Shared_util.roundis(state.dims.size.w))
      height=(Shared_util.roundis(state.dims.size.h))
      style=(Rect.toStyle(rect))
      onClick=(evt => send(getClick(evt)))
      onMouseMove=(evt => send(getHover(evt)))
    />,
};
