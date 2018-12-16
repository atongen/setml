open Belt;

let rows = 10;

let columns = 9;

let cards = Card.deck();

let makeGrid = blockSize => {
  let width = blockSize *. float_of_int(columns);
  let height = blockSize *. float_of_int(rows);
  Grid.make(~width, ~height, ~columns, ~rows, cards);
};

/* render non-visible grid of all set game cards */
let render = (ctx, grid, theme) => {
  CanvasUtils.clear(ctx);
  Grid.flatMap(
    grid,
    (rect, card) => {
      let svgs = Theme.make_card_svgs(~width=rect.Rect.w, ~height=rect.h, ~theme, card);
      List.map(svgs, svg => CanvasUtils.drawSvgImagePromise(svg, ctx, rect));
    },
  )
  |> List.toArray;
};

let boardCardBorderColor = (theme, selected, hovered) =>
  switch (theme) {
  | Theme.Classic
  | Open_source =>
    if (selected && hovered) {
      "#900c3f";
    } else if (selected) {
      "#c70039";
    } else if (hovered) {
      "#d6d4cb";
    } else {
      "white";
    }
  };

let renderBoardCard = (srcCtx, srcRect, dstCtx, dstRect, theme, border, selected, hovered) => {
  let boarderColor = boardCardBorderColor(theme, selected, hovered);
  switch (theme) {
  | Theme.Classic
  | Open_source =>
    CanvasUtils.drawRoundRect(dstCtx, dstRect, border, boarderColor, None);
    CanvasUtils.drawCanvas(srcCtx, srcRect, dstCtx, dstRect);
  };
};

let renderOuterBoard = (ctx, rect, theme, border) =>
  switch (theme) {
  | Theme.Classic
  | Open_source => CanvasUtils.drawRoundRect(ctx, rect, border, Theme.palette(theme).primary, None)
  };

let renderBoard = (srcCtx, srcGrid, dstCtx, dstGrid, theme, status, selected, hovered) => {
  CanvasUtils.reset(dstCtx, "#fafafa");
  let outerRect = Grid.paddedRect(dstGrid);
  let noneCardIdx = Card.to_int_opt(None);
  renderOuterBoard(dstCtx, outerRect, theme, dstGrid.border);
  Grid.forEachWithIndex(
    dstGrid,
    (dstRect, maybeBcd, idx) => {
      let (cardIdx, isSelected, isHovered) =
        switch (maybeBcd) {
        | Some((bcd: Messages.board_card_data)) =>
          assert (bcd.idx == idx);
          let isSelected = Selected.has(selected, bcd);
          let isHovered =
            switch (hovered) {
            | Some(h) => h == bcd
            | None => false
            };
          switch (status) {
          | Game_status.New => (noneCardIdx, isSelected, isHovered)
          | Started
          | Complete => (Card.to_int_opt(bcd.card), isSelected, isHovered)
          };
        | None => (noneCardIdx, false, false)
        };
      switch (Grid.findKeyByIdx(srcGrid, cardIdx)) {
      | Some(srcRect) =>
        renderBoardCard(srcCtx, srcRect, dstCtx, dstRect, theme, dstGrid.border, isSelected, isHovered)
      | None => ()
      };
    },
  );
};
