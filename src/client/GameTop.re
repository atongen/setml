open Belt;

type canvas;

let window = Webapi.Dom.window;

[@bs.send] external getContext : (canvas, string) => Canvas2dRe.t = "";

let devicePixelRatio = () : float => [%bs.raw {| window.devicePixelRatio || 1 |}];

let size_of_ratio = (size, ratio) => int_of_float(floor(float_of_int(size) *. ratio +. 0.5));

type screen = {
  width: int,
  height: int,
  ratio: float,
};

let getScreen = () => {
  width: Webapi.Dom.Window.innerWidth(window),
  height: Webapi.Dom.Window.innerHeight(window),
  ratio: devicePixelRatio(),
};

type action =
  | SendMessage(string)
  | ReceiveMessage(string)
  | Resize;

type state = {
  messages: list(string),
  ws: ref(option(WebSockets.WebSocket.t)),
  screen,
  context: ref(option(Canvas2dRe.t)),
};

let component = ReasonReact.reducerComponent("Game");

let make = _children => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | ReceiveMessage(message) =>
      let messages = state.messages @ [message];
      ReasonReact.Update({...state, messages});
    | SendMessage(message) =>
      switch (state.ws) {
      | {contents: Some(ws)} => WebSockets.WebSocket.sendString(message, ws)
      | _ => Js.log("Unable to send: No websocket connection!")
      };
      ReasonReact.NoUpdate;
    | Resize =>
      Js.log("resizing");
      ReasonReact.Update({...state, screen: getScreen()});
    },
  initialState: () => {messages: [], ws: ref(None), screen: getScreen(), context: ref(None)},
  didMount: self => {
    let myCanvas: canvas = [%bs.raw {| document.getElementById("myCanvas") |}];
    let context = getContext(myCanvas, "2d");
    self.state.context := Some(context);
    let handleMessage = evt => {
      let str = WebSockets.MessageEvent.stringData(evt);
      self.send(ReceiveMessage(str));
      switch (self.state.context) {
      | {contents: Some(ctx)} =>
        Canvas2dRe.setFillStyle(ctx, Canvas2dRe.String, "red");
        Canvas2dRe.fillRect(~x=25.0, ~y=75.0, ~h=100.0, ~w=100.0, ctx);
      | _ => Js.log("Unable to draw: No canvas context!")
      };
    };
    switch (Util.ws_url()) {
    | Some(ws_url) =>
      let ws = WebSockets.WebSocket.make(ws_url);
      ws |> WebSockets.WebSocket.on(WebSockets.WebSocket.Message(handleMessage)) |> ignore;
      self.state.ws := Some(ws);
    | None => Js.log("Unable to get websocket url!")
    };
    let handleResize = (_) => self.send(Resize);
    WindowRe.addEventListener("resize", handleResize, window);
    ReasonReact.NoUpdate;
  },
  render: ({state, send}) =>
    /*let messages =
      state.messages
      |> List.mapWithIndex(_, (i, message) =>
           <li key=(string_of_int(i))> (ReasonReact.stringToElement(message)) </li>
         ); */
    <section className="main">
      /*<button onClick=(_event => send(SendMessage("Did it.")))> (ReasonReact.stringToElement("Do it.")) </button>
        <ul className="messages"> (ReasonReact.arrayToElement(List.toArray(messages))) </ul>*/

        <Board
          name="myCanvas"
          width=(size_of_ratio(state.screen.width, state.screen.ratio))
          height=(size_of_ratio(state.screen.height, state.screen.ratio))
        />
      </section>,
};
