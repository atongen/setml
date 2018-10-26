open Belt;

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
  xOffset: float,
  yOffset: float,
  blockSize: float,
};

type state = {
  dims: dimensions,
  hovered: option(Messages.board_card_data),
  selected: Selected.t,
  context: ref(option(Canvas2dRe.t)),
  renderContext: ref(option(Canvas2dRe.t)),
  boardCards: list(Messages.board_card_data),
  game: Messages.game_update_data,
};

let component = ReasonReact.reducerComponent("Board");

let getClick = evt =>
  Click((float_of_int(ReactEvent.Mouse.clientX(evt)), float_of_int(ReactEvent.Mouse.clientY(evt))));

let getHover = evt =>
  Hover((float_of_int(ReactEvent.Mouse.clientX(evt)), float_of_int(ReactEvent.Mouse.clientY(evt))));

let blockColor = idx => {
  let m = 207126.1234567901;
  /* 81 * m = 16777216 = 256^3 */
  let n = Shared_util.roundi(float_of_int(idx) *. m);
  let a = Base_conv.base_list_of_int(~base=256, ~size=3, n) |> List.toArray;
  let f = i => Array.getUnsafe(a, i) |> string_of_int;
  let (r, g, b) = (f(0), f(1), f(2));
  "rgb(" ++ r ++ ", " ++ g ++ ", " ++ b ++ ")";
};

let boardCardColor = maybeCard => {
  let idx =
    switch (maybeCard) {
    | Some(card) => Card.to_int(card)
    | None => 81
    };
  blockColor(idx);
};

let blockStroke = (selected, hovered) =>
  if (selected && hovered) {
    Some("purple");
  } else if (selected) {
    Some("red");
  } else if (hovered) {
    Some("blue");
  } else {
    None;
  };

let drawBlock = (ctx, color, idx, rect, cardOpt, border, selected, hovered) => {
  let shrinkRect = Rect.shrink(rect, border);
  CanvasUtils.drawRoundRect(ctx, shrinkRect, 5.0, color, blockStroke(selected, hovered));
  Canvas2dRe.font(ctx, "24px serif");
  let text = Printf.sprintf("%d (%d)", idx, Card.to_int_opt(cardOpt));
  Canvas2dRe.strokeText(text, ctx, ~x=rect.x +. 30., ~y=rect.y +. 30.);
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

let border = (width, height) => min(width, height) /. 50.;

let getBoardCard = (boardCards: list(Messages.board_card_data), idx) =>
  switch (List.getBy(boardCards, bcd => bcd.idx == idx)) {
  | Some(bcd) => Some(bcd)
  | None => None
  };

let drawBoard = (ctx, dims, cards: list(Messages.board_card_data), selected, hovered) => {
  CanvasUtils.reset(ctx, "white");
  let cardBoarder = min(dims.border.x, dims.border.y);
  CanvasUtils.drawRoundRect(ctx, dims.border, 5.0, "#3f51b5", None);
  for (i in 0 to dims.rows - 1) {
    for (j in 0 to dims.columns - 1) {
      let idx = i * dims.columns + j;
      switch (dims.blocks[idx], getBoardCard(cards, idx)) {
      | (Some(rect), Some(bcd)) =>
        let is_selected = Selected.has(selected, bcd);
        let is_hovered =
          switch (hovered) {
          | Some(h) => h == bcd
          | None => false
          };
        drawBlock(ctx, boardCardColor(bcd.card), idx, rect, bcd.card, cardBoarder, is_selected, is_hovered);
      | (Some(rect), None) =>
        /* Js.log("drawBoard error: No board card provided at idx" ++ string_of_int(idx) ++ "!"); */
        drawBlock(ctx, boardCardColor(None), idx, rect, None, cardBoarder, false, false)
      | (None, _c) => Js.log("drawBoard error: No block found at idx " ++ string_of_int(idx) ++ "!")
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
    xOffset: xOffset +. b,
    yOffset: yOffset +. b,
    blockSize: bs,
  };
};

let printSets = (boardCards: list(Messages.board_card_data), theme) => {
  let sets = Messages_util.board_cards_sets(boardCards);
  let s = c => Theme.card_to_string(theme, c);
  List.forEach(sets, ((c0, c1, c2)) =>
    Js.log(
      Printf.sprintf("Set: (%d: %s, %d: %s, %d: %s)", c0.idx, s(c0.card), c1.idx, s(c1.card), c2.idx, s(c2.card)),
    )
  );
};

let shouldRedraw = (oldState: state, newState: state) =>
  if (oldState.dims.size != newState.dims.size) {
    true;
  } else if (oldState.boardCards != newState.boardCards) {
    true;
  } else if (Selected.is_symmetric_diff(oldState.selected, newState.selected)) {
    true;
  } else if (! Option.eq(oldState.hovered, newState.hovered, (a, b) => a == b)) {
    true;
  } else {
    false;
  };

let printSelections = selected => Js.log(Selected.to_string(selected));

let make = (_children, ~rect, ~ratio, ~columns, ~rows, ~boardCards, ~game, ~sendMessage, ~boardOffsetX, ~boardOffsetY) => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | Click((x, y)) =>
      switch (Rect.findRect(state.dims.blocks, (x -. boardOffsetX, y -. boardOffsetY))) {
      | Some(idx) =>
        switch (getBoardCard(state.boardCards, idx)) {
        | Some(bcd) =>
          if (Selected.has(state.selected, bcd)) {
            let newSelected = Selected.remove(state.selected, bcd);
            printSelections(newSelected);
            ReasonReact.Update({...state, selected: newSelected});
          } else {
            let newSelected = Selected.add(state.selected, bcd);
            let l = Selected.size(newSelected);
            if (l < 3) {
              printSelections(newSelected);
              ReasonReact.Update({...state, selected: newSelected});
            } else {
              let l = Selected.to_list(newSelected);
              switch (Messages_util.board_cards_list_is_set(l)) {
              | Some((cd0, cd1, cd2)) =>
                sendMessage(ClientUtil.make_move_msg(cd0, cd1, cd2));
                Js.log("You got a set!");
              | None => Js.log("That's not a set, dummy!")
              };
              printSelections(Selected.empty);
              ReasonReact.Update({...state, selected: Selected.empty});
            };
          }
        | None => ReasonReact.NoUpdate
        }
      | None => ReasonReact.NoUpdate
      }
    | Hover((x, y)) =>
      let maybeNewHoveredIdx = Rect.findRect(state.dims.blocks, (x -. boardOffsetX, y -. boardOffsetY));
      let maybeNewHovered =
        switch (maybeNewHoveredIdx) {
        | Some(newHoveredIdx) => getBoardCard(state.boardCards, newHoveredIdx)
        | None => None
        };
      switch (state.hovered, maybeNewHovered) {
      | (Some(oldHovered), Some(newHovered)) =>
        if (oldHovered == newHovered) {
          ReasonReact.NoUpdate;
        } else {
          ReasonReact.Update({...state, hovered: Some(newHovered)});
        }
      | (Some(_oldHovered), None) => ReasonReact.Update({...state, hovered: None})
      | (None, Some(newHovered)) => ReasonReact.Update({...state, hovered: Some(newHovered)})
      | (None, None) => ReasonReact.NoUpdate
      };
    },
  initialState: () => {
    dims: makeDims(rect, ratio, columns, rows),
    hovered: None,
    selected: Selected.empty,
    context: ref(None),
    renderContext: ref(None),
    boardCards,
    game,
  },
  willReceiveProps: self => {...self.state, dims: makeDims(rect, ratio, columns, rows), boardCards, game},
  didMount: self => {
    let context = CanvasUtils.getContext("board");
    let renderContext = CanvasUtils.getContext("card-render");
    self.state.context := Some(context);
    self.state.renderContext := Some(renderContext);
    ();
  },
  didUpdate: ({oldSelf, newSelf}) =>
    if (shouldRedraw(oldSelf.state, newSelf.state)) {
      printSets(newSelf.state.boardCards, newSelf.state.game.theme);
      switch ((newSelf.state.context, newSelf.state.renderContext)) {
      | ({contents: Some(ctx)}, {contents: Some(_rndCtx)}) =>
        drawBoard(ctx, newSelf.state.dims, newSelf.state.boardCards, newSelf.state.selected, newSelf.state.hovered);
      | _ => Js.log("Unable to redraw blocks: No context found!")
      };
    },
  render: ({state, send}) => {
    <div>
      <CardRender id="card-render" blockSize=state.dims.blockSize theme=state.game.theme />
      <canvas
        id="board"
        width=(Shared_util.roundis(state.dims.size.w))
        height=(Shared_util.roundis(state.dims.size.h))
        style=(Rect.toStyle(rect, ()))
        onClick=(evt => send(getClick(evt)))
        onMouseMove=(evt => send(getHover(evt)))
      />
    </div>
  },
};
