open Belt;

type canvas;

[@bs.send] external getContext : (canvas, string) => Canvas2dRe.t = "";

module Game = {
  type action =
    | SendMessage(string)
    | ReceiveMessage(string);
  type screen = {
    width: int,
    height: int,
    ratio: int,
  };
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
        | {contents: Some(ws)} =>
          WebSockets.WebSocket.sendString(message, ws)
        | _ => Js.log("Unable to send: No websocket connection!")
        };
        ReasonReact.NoUpdate;
      },
    initialState: () => {
      messages: [],
      ws: ref(None),
      screen: {
        width: 800,
        height: 600,
        ratio: 1,
      },
      context: ref(None),
    },
    didMount: self => {
      let myCanvas: canvas = [%bs.raw
        {| document.getElementById("mycanvas") |}
      ];
      let context = getContext(myCanvas, "2d");
      self.state.context := Some(context);
      let handleMessage = evt => {
        let str = WebSockets.MessageEvent.stringData(evt);
        self.send(ReceiveMessage(str));
      };
      switch (Util.ws_url()) {
      | Some(ws_url) =>
        let ws = WebSockets.WebSocket.make(ws_url);
        ws
        |> WebSockets.WebSocket.on(
             WebSockets.WebSocket.Message(handleMessage),
           )
        |> ignore;
        self.state.ws := Some(ws);
      | None => Js.log("Unable to get websocket url!")
      };
      ReasonReact.NoUpdate;
    },
    render: ({state, send}) => {
      let messages =
        state.messages
        |> List.map(_, message =>
             <li> (ReasonReact.stringToElement(message)) </li>
           );
      <section className="main">
        <button onClick=(_event => send(SendMessage("Did it.")))>
          (ReasonReact.stringToElement("Do it."))
        </button>
        <ul className="messages">
          (ReasonReact.arrayToElement(List.toArray(messages)))
        </ul>
        <canvas
          id="myCanvas"
          width=(string_of_int(state.screen.width * state.screen.ratio))
          height=(string_of_int(state.screen.height * state.screen.ratio))
        />
      </section>;
    },
  };
};

ReactDOMRe.renderToElementWithId(<Game />, "game");
