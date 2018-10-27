open Belt;

let rows = 10;

let columns = 9;

let makeRect = blockSize =>
  Rect.make(0.0, 0.0, blockSize *. float_of_int(columns), blockSize *. float_of_int(rows));

let cardColor = idx => {
  let m = 207126.1234567901;
  /* 81 * m = 16777216 = 256^3 */
  let n = Shared_util.roundi(float_of_int(idx) *. m);
  let a = Base_conv.base_list_of_int(~base=256, ~size=3, n) |> List.toArray;
  let f = i => Array.getUnsafe(a, i) |> string_of_int;
  let (r, g, b) = (f(0), f(1), f(2));
  "rgb(" ++ r ++ ", " ++ g ++ ", " ++ b ++ ")";
};

let renderCards = (ctx, blockSize, theme) => {
  Js.log("render");
  CanvasUtils.clear(ctx);
  let deck = Card.deck();
  for (i in 0 to rows - 1) {
    for (j in 0 to columns - 1) {
      let idx = i * columns + j;
      let block = Rect.make(float_of_int(j) *. blockSize, float_of_int(i) *. blockSize, blockSize, blockSize);
      let shrinkRect = Rect.shrink(block, 5.0);
      if (idx >= 0 && idx < 81) {
        CanvasUtils.drawRoundRect(ctx, shrinkRect, 5.0, cardColor(idx), Some("blue"));
        let card = Array.getExn(deck, idx);
        let cardDesc = Theme.card_to_string(theme, card);
        Canvas2dRe.font(ctx, "24px serif");
        let text = Printf.sprintf("%d %s", idx, cardDesc);
        Canvas2dRe.strokeText(text, ctx, ~x=block.x +. 30., ~y=block.y +. 30.);
      } else {
        CanvasUtils.drawRoundRect(ctx, shrinkRect, 5.0, "yellow", Some("green"));
      };
    };
  };
};
