open Belt;

let window = Webapi.Dom.window;

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
};

let handleMessage = (evt, self) => {
  let str = WebSockets.MessageEvent.stringData(evt);
  self.ReasonReact.send(ReceiveMessage(str));
};

let handleResize = (evt, self) => self.ReasonReact.send(Resize);

let component = ReasonReact.reducerComponent("Game");

let make = _children => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | ReceiveMessage(message) =>
      Js.log("Got message: " ++ message);
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
  initialState: () => {messages: [], ws: ref(None), screen: getScreen()},
  didMount: self => {
    switch (Util.ws_url()) {
    | Some(ws_url) =>
      let ws = WebSockets.WebSocket.make(ws_url);
      self.state.ws := Some(ws);
    | None => Js.log("Unable to get websocket url!")
    };
    ReasonReact.NoUpdate;
  },
  subscriptions: self => [
    Sub(
      () => WindowRe.addEventListener("resize", self.handle(handleResize), window),
      (_) => WindowRe.removeEventListener("resize", self.handle(handleResize), window),
    ),
    Sub(
        () => switch(self.state.ws) {
        | {contents: Some(ws)} =>
            WebSockets.WebSocket.(
                ws |> on(Message(self.handle(handleMessage))) |> ignore
            );
        | _ => Js.log("Unable to setup websocket message handler");
        },
        () => (),
    ),
  ],
  render: ({state, send}) =>
    <section className="main">
        <Board
          name="myCanvas"
          width=(size_of_ratio(state.screen.width, state.screen.ratio))
          height=(size_of_ratio(state.screen.height, state.screen.ratio))
        />
      </section>,
};
