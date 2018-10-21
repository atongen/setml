type canvas;

type document;

[@bs.val] external doc : document = "document";

[@bs.send] external getCanvasById : (document, string) => canvas = "getElementById";

[@bs.send] external getContext : (canvas, string) => Canvas2dRe.t = "";
[@bs.get] external canvasFromCtx : Canvas2dRe.t => canvas = "canvas";

[@bs.get] external width : canvas => float = "clientWidth";
[@bs.get] external height : canvas => float = "clientHeight";

let ctxSize = ctx => {
    let canvas = canvasFromCtx(ctx);
    (width(canvas), height(canvas));
};

let getContext = id => {
  let canvas = getCanvasById(doc, id);
  getContext(canvas, "2d");
};

let drawRectangle = (ctx, x, y, w, h, inFillStyle) => {
  open Canvas2dRe;
  let (ft, fs) = fillStyle(ctx);
  setFillStyle(ctx, String, inFillStyle);
  fillRect(~x, ~y, ~w, ~h, ctx);
  setFillStyle(ctx, ft, fs);
};

let drawRect = (ctx, rect, fillStyle) => drawRectangle(ctx, rect.Rect.x, rect.y, rect.w, rect.h, fillStyle);

let reset = (ctx, fillStyle) => {
    let (width, height) = ctxSize(ctx);
    drawRectangle(ctx, 0.0, 0.0, width, height, fillStyle);
};

let fill = (ctx, inFillStyle) => {
  open Canvas2dRe;
  let (ft, fs) = fillStyle(ctx);
  setFillStyle(ctx, String, inFillStyle);
  fill(ctx);
  setFillStyle(ctx, ft, fs);
};

let stroke = (ctx, inStrokeStyle) => {
  open Canvas2dRe;
  let (st, ss) = strokeStyle(ctx);
  setStrokeStyle(ctx, String, inStrokeStyle);
  stroke(ctx);
  setStrokeStyle(ctx, st, ss);
};

let drawRoundRectangle = (ctx, x, y, w, h, radius, fillStyle, maybeStrokeStyle) => {
  Canvas2dRe.beginPath(ctx);
  Canvas2dRe.moveTo(ctx, ~x=x +. radius, ~y);
  Canvas2dRe.lineTo(ctx, ~x=x +. w -. radius, ~y);
  Canvas2dRe.quadraticCurveTo(ctx, ~cp1x=x +. w, ~cp1y=y, ~x=x +. w, ~y=y +. radius);
  Canvas2dRe.lineTo(ctx, ~x=x +. w, ~y=y +. h -. radius);
  Canvas2dRe.quadraticCurveTo(ctx, ~cp1x=x +. w, ~cp1y=y +. h, ~x=x +. w -. radius, ~y=y +. h);
  Canvas2dRe.lineTo(ctx, ~x=x +. radius, ~y=y +. h);
  Canvas2dRe.quadraticCurveTo(ctx, ~cp1x=x, ~cp1y=y +. h, ~x, ~y=y +. h -. radius);
  Canvas2dRe.lineTo(ctx, ~x, ~y=y +. radius);
  Canvas2dRe.quadraticCurveTo(ctx, ~cp1x=x, ~cp1y=y, ~x=x +. radius, ~y);
  Canvas2dRe.closePath(ctx);
  fill(ctx, fillStyle);
  switch(maybeStrokeStyle) {
  | Some(strokeStyle) => stroke(ctx, strokeStyle);
  | None => ()
  };
};

let drawRoundRect = (ctx, rect, radius, fillStyle, strokeStyle) => {
  drawRoundRectangle(ctx, rect.Rect.x, rect.y, rect.w, rect.h, radius, fillStyle, strokeStyle);
};
