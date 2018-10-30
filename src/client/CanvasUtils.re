type canvas;

type document;

[@bs.val] external doc : document = "document";

[@bs.send] external getCanvasById : (document, string) => canvas = "getElementById";

[@bs.send] external getContext : (canvas, string) => Canvas2dRe.t = "";

[@bs.get] external canvasFromCtx : Canvas2dRe.t => canvas = "canvas";

[@bs.get] external width : canvas => float = "clientWidth";

[@bs.get] external height : canvas => float = "clientHeight";

[@bs.get] external getLineWidth : Canvas2dRe.t => float = "lineWidth";

[@bs.send]
external drawCanvasImage :
  (
    Canvas2dRe.t,
    ~image: canvas,
    ~sx: float,
    ~sy: float,
    ~sWidth: float,
    ~sHeight: float,
    ~dx: float,
    ~dy: float,
    ~dWidth: float,
    ~dHeight: float
  ) =>
  unit =
  "drawImage";

[@bs.send]
external drawImageImage :
  (
    Canvas2dRe.t,
    ~image: HtmlImageElementRe.t,
    ~sx: float,
    ~sy: float,
    ~sWidth: float,
    ~sHeight: float,
    ~dx: float,
    ~dy: float,
    ~dWidth: float,
    ~dHeight: float
  ) =>
  unit =
  "drawImage";

[@bs.send]
external drawImageSimple : (Canvas2dRe.t, ~image: HtmlImageElementRe.t, ~sx: float, ~sy: float) => unit = "drawImage";

[@bs.send.pipe: canvas] external getBoundingClientRect : Dom.domRect = "";

[@bs.set] external setImageSrc : (HtmlImageElementRe.t, string) => unit = "src";

[@bs.set] external setOnload : (HtmlImageElementRe.t, unit => unit) => unit = "onload";

let getCanvas = id => getCanvasById(doc, id);

let offset = canvas => {
  let rect = getBoundingClientRect(canvas);
  (float_of_int(DomRectRe.left(rect)), float_of_int(DomRectRe.top(rect)));
};

let ctxSize = ctx => {
  let canvas = canvasFromCtx(ctx);
  (width(canvas), height(canvas));
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

let clear = ctx => {
  let (width, height) = ctxSize(ctx);
  drawRectangle(ctx, 0.0, 0.0, width, height, "rgba(0, 0, 0, 0.0)");
};

let fill = (ctx, inFillStyle) => {
  open Canvas2dRe;
  let (ft, fs) = fillStyle(ctx);
  setFillStyle(ctx, String, inFillStyle);
  fill(ctx);
  setFillStyle(ctx, ft, fs);
};

let stroke = (~inStrokeStyle, ~inLineWidth=5.0, ctx) => {
  open Canvas2dRe;
  let (st, ss) = strokeStyle(ctx);
  let lw = getLineWidth(ctx);
  setStrokeStyle(ctx, String, inStrokeStyle);
  lineWidth(ctx, inLineWidth);
  stroke(ctx);
  setStrokeStyle(ctx, st, ss);
  lineWidth(ctx, lw);
};

/* https://stackoverflow.com/questions/1255512/how-to-draw-a-rounded-rectangle-on-html-canvas */
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
  switch (maybeStrokeStyle) {
  | Some(strokeStyle) => stroke(~inStrokeStyle=strokeStyle, ctx)
  | None => ()
  };
};

let drawRoundRect = (ctx, rect, radius, fillStyle, strokeStyle) =>
  drawRoundRectangle(ctx, rect.Rect.x, rect.y, rect.w, rect.h, radius, fillStyle, strokeStyle);

let drawCanvas = (srcCtx, srcRect, dstCtx, dstRect) =>
  drawCanvasImage(
    dstCtx,
    ~image=canvasFromCtx(srcCtx),
    ~sx=srcRect.Rect.x,
    ~sy=srcRect.y,
    ~sWidth=srcRect.w,
    ~sHeight=srcRect.h,
    ~dx=dstRect.Rect.x,
    ~dy=dstRect.y,
    ~dWidth=dstRect.w,
    ~dHeight=dstRect.h,
  );

let drawSvgImage = (svg, ctx, rect) => {
  let image = HtmlImageElementRe.makeWithSize(int_of_float(rect.Rect.w), int_of_float(rect.h));
  setOnload(image, () => drawImageSimple(ctx, ~image, ~sx=rect.Rect.x, ~sy=rect.y));
  setImageSrc(image, Printf.sprintf("data:image/svg+xml;utf8,%s", svg));
};
