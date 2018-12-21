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

let evtPoint = (evt, {contents: (offsetX, offsetY)}) => (
  float_of_int(ReactEvent.Mouse.clientX(evt)) -. offsetX,
  float_of_int(ReactEvent.Mouse.clientY(evt)) -. offsetY,
);

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

type redraw =
  | Cards
  | Board(Set.Int.t)
  | NoChange;

let shouldRedraw = (oldState: state, newState: state) =>
  if (oldState.boardGrid.width != newState.boardGrid.width || oldState.boardGrid.height != newState.boardGrid.height) {
    Cards;
  } else if (oldState.game.theme != newState.game.theme) {
    Cards;
  } else if (oldState.game.status != newState.game.status) {
    Cards;
  } else if (oldState.boardGrid.values != newState.boardGrid.values) {
    let oldValues = Selected.of_array(oldState.boardGrid.values);
    let newValues = Selected.of_array(newState.boardGrid.values);
    let diff = Selected.symmetric_diff(oldValues, newValues);
    Board(Selected.indexes(diff));
  } else if (Selected.is_symmetric_diff(oldState.selected, newState.selected)) {
    let idxs = Selected.union(oldState.selected, newState.selected) |> Selected.indexes;
    Board(idxs);
  } else if (! Option.eq(oldState.hovered, newState.hovered, (a, b) => a == b)) {
    let idxs =
      List.reduce([oldState.hovered, newState.hovered], Set.Int.empty, (s, o) =>
        switch (o) {
        | Some(bcd) => Set.Int.add(s, bcd.idx)
        | None => s
        }
      );
    Board(idxs);
  } else {
    NoChange;
  };

let renderAllBoard = (srcCtx, dstCtx, state) =>
  CardRender.renderAllBoard(
    srcCtx,
    state.cardGrid,
    dstCtx,
    state.boardGrid,
    state.game.theme,
    state.game.status,
    state.selected,
    state.hovered,
  );

let renderSomeBoard = (srcCtx, dstCtx, state, cardIdxs) =>
  CardRender.renderSomeBoard(
    ~cardIdxs,
    srcCtx,
    state.cardGrid,
    dstCtx,
    state.boardGrid,
    state.game.theme,
    state.game.status,
    state.selected,
    state.hovered,
  );

let renderCards = (srcCtx, dstCtx, state) =>
  Js.Promise.(
    all(CardRender.render(srcCtx, state.cardGrid, state.game.theme))
    |> then_(_results => {
         /* printSets(state.boardGrid.values, state.game.theme); */
         renderAllBoard(srcCtx, dstCtx, state);
         resolve();
       })
  );

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
            let ns =
              if (len == 3) {
                let l = Selected.to_list(newSelected);
                switch (Messages_util.board_cards_list_is_set(l)) {
                | Some((cd0, cd1, cd2)) =>
                  sendMessage(ClientUtil.make_move_msg(cd0, cd1, cd2));
                  newSelected;
                | None => Selected.empty
                };
              } else if (len > 3) {
                Selected.empty;
              } else {
                newSelected;
              };
            ReasonReact.Update({...state, selected: ns});
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
    let selected =
      if (List.toArray(boardCards) == self.state.boardGrid.values) {
        /* board has not changed, keep selected the same */
        self.state.
          selected;
      } else {
        /* board has changed, reset selected */
        Selected.empty;
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
    renderCards(renderContext, boardContext, self.state) |> ignore;
  },
  didUpdate: ({oldSelf, newSelf}) =>
    switch (shouldRedraw(oldSelf.state, newSelf.state)) {
    | Cards =>
      switch (newSelf.state.context, newSelf.state.renderContext) {
      | ({contents: Some(dstCtx)}, {contents: Some(srcCtx)}) =>
        renderCards(srcCtx, dstCtx, newSelf.state) |> ignore
      | _ => Js.log("Unable to render cards: No context found!")
      }
    | Board(cardIdxs) =>
      switch (newSelf.state.context, newSelf.state.renderContext) {
      | ({contents: Some(dstCtx)}, {contents: Some(srcCtx)}) =>
        renderSomeBoard(srcCtx, dstCtx, newSelf.state, cardIdxs) |> ignore
      | _ => Js.log("Unable to render some board: No context found!")
      }
    | NoChange => ()
    },
  render: ({state, send}) => {
    let renderRect = Grid.outerRect(state.cardGrid);
    let style =
      ClientUtil.rectToStyle(
        rect,
        ~padding="0",
        ~margin="0",
        ~display="block",
        ~backgroundColor="#fafafa",
        ~position="fixed",
        (),
      );
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
