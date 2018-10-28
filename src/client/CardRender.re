open Belt;

let rows = 10;

let columns = 9;

let makeGrid = blockSize => {
  let width = blockSize *. float_of_int(columns);
  let height = blockSize *. float_of_int(rows);
  Grid.make(~width, ~height, ~columns, ~rows, Card.deck());
};

let cardColor = idx => {
  let i =
    if (idx < 0 || idx > 80) {
      81;
    } else {
      idx;
    };
  let m = 207126.1234567901;
  /* 81 * m = 16777216 = 256^3 */
  let n = Shared_util.roundi(float_of_int(i) *. m);
  let a = Base_conv.base_list_of_int(~base=256, ~size=3, n) |> List.toArray;
  let f = i => Array.getUnsafe(a, i) |> string_of_int;
  let (r, g, b) = (f(0), f(1), f(2));
  "rgb(" ++ r ++ ", " ++ g ++ ", " ++ b ++ ")";
};

let render = (ctx, grid, theme) => {
  CanvasUtils.clear(ctx);
  Grid.forEachWithIndex(grid, (rect, maybeCard, idx) =>
    switch (maybeCard) {
    | Some(card) =>
      let cardIdx = Card.to_int(card);
      CanvasUtils.drawRoundRect(ctx, rect, 10.0, cardColor(cardIdx), None);
      let cardDesc = Theme.card_to_string(~theme, card);
      Canvas2dRe.font(ctx, "24px serif");
      let text = Printf.sprintf("%d %s", idx, cardDesc);
      Canvas2dRe.strokeText(text, ctx, ~x=rect.x +. 30., ~y=rect.y +. 30.);
    | None => ()
    }
  );
};
