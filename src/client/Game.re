open Belt;

module Game = {
  type action =
    | SendMessage(string)
    | ReceiveMessage(string);
  type state = {
    messages: list(string),
    ws: ref(option(WebSockets.WebSocket.t)),
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
    initialState: () => {messages: [], ws: ref(None)},
    didMount: self => {
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
      </section>;
    },
  };
};

ReactDOMRe.renderToElementWithId(<Game />, "game");
