type canvas;
type document;

[@bs.val] external doc : document = "document";
[@bs.send] external getCanvasById: (document, string) => canvas = "getElementById";
[@bs.send] external getContext : (canvas, string) => Canvas2dRe.t = "";

let getContext = id => {
    let canvas = getCanvasById(doc, id)
    getContext(canvas, "2d");
};

let drawRectangle = (ctx, color, x, y, w, h) => {
  Canvas2dRe.setFillStyle(ctx, Canvas2dRe.String, color);
  Canvas2dRe.fillRect(
    ~x=Shared_util.round(x),
    ~y=Shared_util.round(y),
    ~w=Shared_util.round(w),
    ~h=Shared_util.round(h),
    ctx,
  );
};

let drawRect = (ctx, color, rect) => drawRectangle(ctx, color, rect.Rect.x, rect.y, rect.w, rect.h);

let reset = (ctx, color, width, height) => drawRectangle(ctx, color, 0.0, 0.0, width, height);
