let rows = 10
let columns = 9
let makeRect = blockSize => Rect.make(0.0, 0.0, blockSize *. float_of_int(columns), blockSize *. float_of_int(rows));

let renderCards = (ctx, blockSize, _theme) => {
  CanvasUtils.clear(ctx);
  let _deck = Card.deck ();
  for (i in 0 to rows - 1) {
    for (j in 0 to columns - 1) {
      let idx = i * columns + j;
      let block = Rect.make(float_of_int(j)*.blockSize, float_of_int(i)*.blockSize, blockSize, blockSize);
      let shrinkRect = Rect.shrink(block, 5.0);
      if (idx >= 0 && idx < 81) {
        CanvasUtils.drawRoundRect(ctx, shrinkRect, 5.0, "red", Some("blue"));
      } else {
        CanvasUtils.drawRoundRect(ctx, shrinkRect, 5.0, "yellow", Some("green"));
      }
    }
  }
};

let component = ReasonReact.statelessComponent("CardRender");

let make = (_children, ~id, ~blockSize, ~theme) => {
  ...component,
  didMount: _self => {
    let context = CanvasUtils.getContext(id);
    renderCards(context, blockSize, theme);
    ();
  },
  render: _self => {
    let rect = makeRect(blockSize);
    <canvas
      id
      width=(Shared_util.roundis(rect.w))
      height=(Shared_util.roundis(rect.h))
      style=(Rect.toStyle(rect, ()))
    />;
  },
};
