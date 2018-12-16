open Belt;

type action =
  | Click((float, float))
  | Hover((float, float));

type state = {
  boardGrid: Grid.t(Messages.board_card_data),
  boardOffset: ref((float, float)),
  cardGrid: Grid.t(Card.t),
  hovered: option(Messages.board_card_data),
  selected: Selected.t,
  context: ref(option(Canvas2dRe.t)),
  renderContext: ref(option(Canvas2dRe.t)),
  game: Messages.game_update_data,
};

let component = ReasonReact.reducerComponent("Board");

let evtPoint = (evt, {contents: (offsetX, offsetY)}) => {
  (
    float_of_int(ReactEvent.Mouse.clientX(evt)) -. offsetX,
    float_of_int(ReactEvent.Mouse.clientY(evt)) -. offsetY,
  );
};

let getClick = (evt, offset) => Click(evtPoint(evt, offset));

let getHover = (evt, offset) => Hover(evtPoint(evt, offset));

let makeBoardGrid = (width, height, columns, rows, boardCards) => {
  let border = ClientUtil.calculateBorder(width, height);
  Grid.make(~border, ~width, ~height, ~columns, ~rows, List.toArray(boardCards));
};

let printSets = (boardCards: array(Messages.board_card_data), theme) => {
  let sets = Messages_util.board_cards_sets(List.fromArray(boardCards));
  let s = c => Theme.card_to_string(~theme, c);
  List.forEach(sets, ((c0, c1, c2)) =>
    Js.log(
      Printf.sprintf("Set: (%d: %s, %d: %s, %d: %s)", c0.idx, s(c0.card), c1.idx, s(c1.card), c2.idx, s(c2.card)),
    )
  );
};

let shouldRedraw = (oldState: state, newState: state) =>
  if (oldState.boardGrid.width != newState.boardGrid.width || oldState.boardGrid.height != newState.boardGrid.height) {
    (true, true);
  } else if (oldState.game.theme != newState.game.theme) {
    (true, true);
  } else if (oldState.game.status != newState.game.status) {
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

let renderBoard = (srcCtx, dstCtx, state) => {
  CardRender.renderBoard(
    srcCtx,
    state.cardGrid,
    dstCtx,
    state.boardGrid,
    state.game.theme,
    state.game.status,
    state.selected,
    state.hovered,
  );
};

let renderCards = (srcCtx, dstCtx, state) => {
  Js.Promise.(
    all(CardRender.render(srcCtx, state.cardGrid, state.game.theme))
    |> then_(_results => {
         /* printSets(state.boardGrid.values, state.game.theme); */
         renderBoard(srcCtx, dstCtx, state);
         resolve();
       })
  );
};

let make = (_children, ~rect, ~columns, ~rows, ~boardCards, ~game, ~sendMessage) => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | Click((x, y)) =>
      switch (Grid.findByPoint(state.boardGrid, (x, y))) {
      | Some(bcd) =>
        switch (bcd.card) {
        | Some(_card) =>
          if (Selected.has(state.selected, bcd)) {
            let newSelected = Selected.remove(state.selected, bcd);
            ReasonReact.Update({...state, selected: newSelected});
          } else {
            let newSelected = Selected.add(state.selected, bcd);
            let len = Selected.size(newSelected);
            if (len == 3) {
              let l = Selected.to_list(newSelected);
              switch (Messages_util.board_cards_list_is_set(l)) {
              | Some((cd0, cd1, cd2)) =>
                sendMessage(ClientUtil.make_move_msg(cd0, cd1, cd2))
                ReasonReact.NoUpdate
              | None => ReasonReact.Update({...state, selected: Selected.empty})
              };
            } else if (len > 3) {
              ReasonReact.Update({...state, selected: Selected.empty});
            } else {
              ReasonReact.Update({...state, selected: newSelected});
            };
          }
        | None => ReasonReact.NoUpdate
        }
      | None => ReasonReact.NoUpdate
      }
    | Hover((x, y)) =>
      let maybeNewHovered = Grid.findByPoint(state.boardGrid, (x, y));
      switch (state.hovered, maybeNewHovered) {
      | (Some(oldHovered), Some(newHovered)) =>
        if (oldHovered == newHovered) {
          ReasonReact.NoUpdate;
        } else {
          switch (newHovered.card) {
          | Some(_card) => ReasonReact.Update({...state, hovered: Some(newHovered)})
          | None => ReasonReact.Update({...state, hovered: None})
          };
        }
      | (Some(_oldHovered), None) => ReasonReact.Update({...state, hovered: None})
      | (None, Some(newHovered)) =>
        switch (newHovered.card) {
        | Some(_card) => ReasonReact.Update({...state, hovered: Some(newHovered)})
        | None => ReasonReact.Update({...state, hovered: None})
        }
      | (None, None) => ReasonReact.NoUpdate
      };
    },
  initialState: () => {
    let boardGrid = makeBoardGrid(rect.Rect.w, rect.h, columns, rows, boardCards);
    {
      boardGrid,
      boardOffset: ref((0.0, 0.0)),
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
    let selected = if (List.toArray(boardCards) == self.state.boardGrid.values) {
      /* board has not changed, keep selected the same */
      self.state.selected
    } else {
      /* board has changed, reset selected */
      Selected.empty
    };
    {...self.state, boardGrid, cardGrid: CardRender.makeGrid(boardGrid.blockSize), selected, game};
  },
  didMount: self => {
    /* card render canvas */
    let renderCanvas = CanvasUtils.getCanvas("card-render");
    let renderContext = CanvasUtils.getContext(renderCanvas, "2d");
    self.state.renderContext := Some(renderContext);
    /* board canvas */
    let boardCanvas = CanvasUtils.getCanvas("board");
    let boardContext = CanvasUtils.getContext(boardCanvas, "2d");
    self.state.context := Some(boardContext);
    /* board offset for click/hover location */
    self.state.boardOffset := CanvasUtils.offset(boardCanvas);
    /* render card canvas */
    renderCards(renderContext, boardContext, self.state);
    ();
  },
  didUpdate: ({oldSelf, newSelf}) => {
    let (redrawCards, redrawBoard) = shouldRedraw(oldSelf.state, newSelf.state);
    if (redrawCards || redrawBoard) {
      switch (newSelf.state.context, newSelf.state.renderContext) {
      | ({contents: Some(dstCtx)}, {contents: Some(srcCtx)}) =>
        if (redrawCards) {
          renderCards(srcCtx, dstCtx, newSelf.state) |> ignore;
        } else if (redrawBoard) {
          renderBoard(srcCtx, dstCtx, newSelf.state);
        }
      | _ => Js.log("Unable to redraw blocks: No context found!")
      };
    };
  },
  render: ({state, send}) => {
    let renderRect = Grid.outerRect(state.cardGrid);
    let style = ClientUtil.rectToStyle(rect, ~padding="0", ~margin="0", ~display="block", ~backgroundColor="#fafafa", ~position="fixed", ());
    <div>
      <canvas
        id="card-render"
        width=(Shared_util.roundis(renderRect.w))
        height=(Shared_util.roundis(renderRect.h))
        style=(ClientUtil.rectToStyle(renderRect, ~display="none", ()))
      />
      <canvas
        id="board"
        width=(Shared_util.roundis(rect.w))
        height=(Shared_util.roundis(rect.h))
        style
        onClick=(evt => send(getClick(evt, state.boardOffset)))
        onMouseMove=(evt => send(getHover(evt, state.boardOffset)))
      />
    </div>;
  },
};
