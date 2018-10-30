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

let defaultViewRect = Rect.make(0.0, 0.0, 200.0, 200.0);

let svg = (width, height, viewRect, content) => {
  let sr = (~re, ~s, str) => Js.String.replaceByRe(re, s, str);
  Printf.sprintf(
    {svg|
      <svg xmlns='http://www.w3.org/2000/svg' width='%f' height='%f' viewBox='%d %d %d %d'>
        %s
      </svg>
    |svg},
    width,
    height,
    int_of_float(viewRect.Rect.x),
    int_of_float(viewRect.y),
    int_of_float(viewRect.w),
    int_of_float(viewRect.h),
    content,
  )
  |> sr(~re=[%re "/>[\\n\\r\\t ]+</g"], ~s="><")
  |> sr(~re=[%re "/[\\n\\r\\t ]+/g"], ~s=" ")
  |> sr(~re=[%re "/\"/g"], ~s="'")
  |> sr(~re=[%re "/#/g"], ~s="%23")
  |> sr(~re=[%re "/</g"], ~s="%3C")
  |> sr(~re=[%re "/>/g"], ~s="%3E")
  |> Js.String.trim;
};

let c = {wow|
  <circle cx="100" cy="100" r="100" fill="#529fca" />
|wow};

let renderCard = (ctx, card, rect, theme) =>
  switch (theme) {
  | Theme.Classic =>
    let svgStr = svg(rect.Rect.w, rect.h, defaultViewRect, c);
    CanvasUtils.drawSvgImage(svgStr, ctx, rect);
    let cardDesc = Theme.card_to_string(~theme, card);
    Canvas2dRe.font(ctx, "24px serif");
    Canvas2dRe.strokeText(cardDesc, ctx, ~x=rect.x +. 30., ~y=rect.y +. 30.);
  | Theme.Open_source =>
    CanvasUtils.drawRoundRect(ctx, rect, 10.0, "white", None);
    let cardDesc = Theme.card_to_string(~theme, card);
    Canvas2dRe.font(ctx, "24px serif");
    Canvas2dRe.strokeText(cardDesc, ctx, ~x=rect.x +. 30., ~y=rect.y +. 30.);
  };

let render = (ctx, grid, theme) => {
  CanvasUtils.clear(ctx);
  Grid.forEach(grid, (rect, maybeCard) =>
    switch (maybeCard) {
    | Some(card) => renderCard(ctx, card, rect, theme)
    | None => ()
    }
  );
};
