open Belt;

let rows = 10;

let columns = 9;

let makeGrid = blockSize => {
  let width = blockSize *. float_of_int(columns);
  let height = blockSize *. float_of_int(rows);
  Grid.make(~width, ~height, ~columns, ~rows, Card.deck());
};

let render = (ctx, grid, theme) => {
  CanvasUtils.clear(ctx);
  Grid.flatMap(grid, (rect, maybeCard) =>
    switch (maybeCard) {
    | Some(card) =>
      let svgs = Theme.make_card_svgs(~width=rect.Rect.w, ~height=rect.h, ~theme, card);
      List.map(svgs, svg => CanvasUtils.drawSvgImagePromise(svg, ctx, rect));
    | None => []
    }
  )
  |> List.toArray;
};
