open Belt;

type action =
  | Click((float, float))
  | Hover((float, float));

type state = {
  boardGrid: Grid.t(Messages.board_card_data),
  boardOffset: ref(option((float, float))),
  cardGrid: Grid.t(Card.t),
  hovered: option(Messages.board_card_data),
  selected: Selected.t,
  context: ref(option(Canvas2dRe.t)),
  renderContext: ref(option(Canvas2dRe.t)),
  game: Messages.game_update_data,
};

let component = ReasonReact.reducerComponent("Board");

let evtPoint = (evt, offset) => {
  let (offsetX, offsetY) = switch (offset) {
    | {contents: Some((x, y))} => (x, y);
    | _ => (0.0, 0.0);
    };
  (float_of_int(ReactEvent.Mouse.clientX(evt)) -. offsetX, float_of_int(ReactEvent.Mouse.clientY(evt)) -. offsetY);
};

let getClick = (evt, offset) => Click(evtPoint(evt, offset));

let getHover = (evt, offset) => Hover(evtPoint(evt, offset));

let cardBorderColor = (selected, hovered, default) =>
  if (selected && hovered) {
    "#900c3f";
  } else if (selected) {
    "#c70039";
  } else if (hovered) {
    "#ff5733";
  } else {
      default
  };

let drawBlock = (srcCtx, srcRect, dstCtx, dstRect, cardIdx, selected, hovered) => {
  CanvasUtils.drawRect(dstCtx, dstRect, cardBorderColor(selected, hovered, CardRender.cardColor(cardIdx)));
  let content = Rect.shrink(~i=5.0, dstRect);
  CanvasUtils.drawCanvas(srcCtx, srcRect, dstCtx, content);
};

let drawBoard = (srcCtx, srcGrid, dstCtx, dstGrid, selected, hovered) => {
  CanvasUtils.reset(dstCtx, "white");
  let outerRect = Grid.paddedRect(dstGrid);
  CanvasUtils.drawRoundRect(dstCtx, outerRect, 5.0, "#3f51b5", None);
  Grid.forEachWithIndex(dstGrid, (dstRect, maybeBcd, idx) => {
    let (cardIdx, isSelected, isHovered) = switch (maybeBcd) {
    | Some((bcd: Messages.board_card_data)) =>
      assert (bcd.idx == idx);
      let isSelected = Selected.has(selected, bcd);
      let isHovered =
        switch (hovered) {
        | Some(h) => h == bcd
        | None => false
        };
        (Card.to_int_opt(bcd.card), isSelected, isHovered)
    | None => (Card.to_int_opt(None), false, false)
    };
    switch(Grid.findKeyByIdx(srcGrid, cardIdx)) {
    | Some(srcRect) => drawBlock(srcCtx, srcRect, dstCtx, dstRect, cardIdx, isSelected, isHovered);
    | None => ();
    }
  });
};

let makeBoardGrid = (width, height, columns, rows, boardCards) => {
  let border = ClientUtil.calculateBorder(width, height);
  Grid.make(~border, ~width, ~height, ~columns, ~rows, List.toArray(boardCards));
};

let printSets = (boardCards: array(Messages.board_card_data), theme) => {
  let sets = Messages_util.board_cards_sets(List.fromArray(boardCards));
  let s = c => Theme.card_to_string(theme, c);
  List.forEach(sets, ((c0, c1, c2)) =>
    Js.log(
      Printf.sprintf("Set: (%d: %s, %d: %s, %d: %s)", c0.idx, s(c0.card), c1.idx, s(c1.card), c2.idx, s(c2.card)),
    )
  );
};

let shouldRedraw = (oldState: state, newState: state) =>
  if (oldState.boardGrid.width != newState.boardGrid.width || oldState.boardGrid.height != newState.boardGrid.height) {
    (true, true);
  } else if (oldState.boardGrid.values != newState.boardGrid.values) {
    (false, true);
  } else if (Selected.is_symmetric_diff(oldState.selected, newState.selected)) {
    (false, true);
  } else if (! Option.eq(oldState.hovered, newState.hovered, (a, b) => a == b)) {
    (false, true);
  } else {
    (false, false);
  };

let printSelections = selected => Js.log(Selected.to_string(selected));

let make = (_children, ~rect, ~columns, ~rows, ~boardCards, ~game, ~sendMessage) => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | Click((x, y)) => {
      switch (Grid.findByPoint(state.boardGrid, (x, y))) {
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
    }
    | Hover((x, y)) => {
      let maybeNewHovered = Grid.findByPoint(state.boardGrid, (x, y));
      switch (state.hovered, maybeNewHovered) {
      | (Some(oldHovered), Some(newHovered)) =>
        if (oldHovered == newHovered) {
          ReasonReact.NoUpdate;
        } else {
          ReasonReact.Update({...state, hovered: Some(newHovered)});
        }
      | (Some(_oldHovered), None) => ReasonReact.Update({...state, hovered: None})
      | (None, Some(newHovered)) => {
          ReasonReact.Update({...state, hovered: Some(newHovered)})
      }
      | (None, None) => ReasonReact.NoUpdate
      };
    }
    },
  initialState: () => {
    let boardGrid = makeBoardGrid(rect.Rect.w, rect.h, columns, rows, boardCards);
    {
      boardGrid,
      boardOffset: ref(None),
      cardGrid: CardRender.makeGrid(boardGrid.blockSize),
      hovered: None,
      selected: Selected.empty,
      context: ref(None),
      renderContext: ref(None),
      game,
    };
  },
  willReceiveProps: self => {
    let boardGrid = makeBoardGrid(rect.Rect.w, rect.h, columns, rows, boardCards);
    {...self.state, boardGrid, cardGrid: CardRender.makeGrid(boardGrid.blockSize), game};
  },
  didMount: self => {
    let boardCanvas = CanvasUtils.getCanvas("board");
    let boardOffset = CanvasUtils.offset(boardCanvas);
    let boardContext = CanvasUtils.getContext(boardCanvas, "2d");
    let renderCanvas = CanvasUtils.getCanvas("card-render");
    let renderContext = CanvasUtils.getContext(renderCanvas, "2d");
    self.state.context := Some(boardContext);
    self.state.renderContext := Some(renderContext);
    self.state.boardOffset := Some(boardOffset);
    CardRender.render(renderContext, self.state.cardGrid, self.state.game.theme);
    ();
  },
  didUpdate: ({oldSelf, newSelf}) => {
    let (redrawCards, redrawBoard) = shouldRedraw(oldSelf.state, newSelf.state);
    if (redrawCards || redrawBoard) {
      switch (newSelf.state.context, newSelf.state.renderContext) {
      | ({contents: Some(dstCtx)}, {contents: Some(srcCtx)}) =>
        if (redrawCards) {
          CardRender.render(srcCtx, newSelf.state.cardGrid, newSelf.state.game.theme);
        }
        if (redrawBoard) {
          printSets(newSelf.state.boardGrid.values, newSelf.state.game.theme);
          drawBoard(srcCtx, newSelf.state.cardGrid, dstCtx, newSelf.state.boardGrid, newSelf.state.selected, newSelf.state.hovered)
        }
      | _ => Js.log("Unable to redraw blocks: No context found!")
      };
    }
  },
  render: ({state, send}) => {
    let renderRect = Grid.outerRect(state.cardGrid);
    <div>
      <canvas
        id="card-render"
        width=(Shared_util.roundis(renderRect.w))
        height=(Shared_util.roundis(renderRect.h))
        style=(Rect.toStyle(renderRect, ~display="none", ()))
      />
      <canvas
        id="board"
        width=(Shared_util.roundis(rect.w))
        height=(Shared_util.roundis(rect.h))
        style=(Rect.toStyle(rect, ()))
        onClick=(evt => send(getClick(evt, state.boardOffset)))
        onMouseMove=(evt => send(getHover(evt, state.boardOffset)))
      />
    </div>;
  },
};
