open Belt;

module Game = {
  type action =
    | Message(string);
  type state = {
    messages: list(string),
    ws: ref(option(WebSockets.WebSocket.t)),
  };
  let component = ReasonReact.reducerComponent("Game");
  let make = _children => {
    ...component,
    reducer: (action, state) =>
      switch (action) {
      | Message(message) =>
        let messages = state.messages @ [message];
        ReasonReact.Update({...state, messages});
      },
    initialState: () => {messages: [], ws: ref(None)},
    didMount: self => {
      let handleMessage = evt => {
        let str = WebSockets.MessageEvent.stringData(evt);
        self.send(Message(str));
      };
      let ws_url = Util.ws_url();
      Js.log("here: " ++ ws_url);
      let ws = WebSockets.WebSocket.make(ws_url);
      ws
      |> WebSockets.WebSocket.on(
           WebSockets.WebSocket.Message(handleMessage),
         )
      |> ignore;
      self.state.ws := Some(ws);
      ReasonReact.NoUpdate;
    },
    render: ({state, send}) => {
      let messages =
        state.messages
        |> List.map(_, message =>
             <li> (ReasonReact.stringToElement(message)) </li>
           );
      <section className="main">
        <button onClick=(_event => send(Message("Did it.")))>
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
