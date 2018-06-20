open Belt;

open Messages;

open ClientMessages;

/*[@bs.val] external requestAnimationFrame : (unit => unit) => unit = "";*/
type action =
  | SendMessage(string)
  | ReceiveMessage(string);

type state = {
  ws: ref(option(WebSockets.WebSocket.t)),
  board: list(Card.t),
  players: list(string),
};

let handleMessage = (evt, self) => {
  let str = WebSockets.MessageEvent.stringData(evt);
  self.ReasonReact.send(ReceiveMessage(str));
};

let rec handleReceiveMessage = (state, msg) => {
  Js.log("wow!");
  ReasonReact.NoUpdate;
};

let updateFrame = self => ();

/* Nothing to do here yet... */
let component = ReasonReact.reducerComponent("Game");

let make = _children => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | ReceiveMessage(message) =>
      let msg = ClientMessageConverter.of_json(message);
      Js.log(Messages.to_string(msg));
      handleReceiveMessage(state, msg);
    | SendMessage(message) =>
      switch (state.ws) {
      | {contents: Some(ws)} => WebSockets.WebSocket.sendString(message, ws)
      | _ => Js.log("Unable to send: No websocket connection!")
      };
      ReasonReact.NoUpdate;
    },
  initialState: () => {ws: ref(None), board: [], players: []},
  didMount: self => {
    switch (ClientUtil.ws_url()) {
    | Some(ws_url) =>
      let ws = WebSockets.WebSocket.make(ws_url);
      self.state.ws := Some(ws);
      WebSockets.WebSocket.(ws |> on(Message(self.handle(handleMessage))) |> ignore);
    | None => Js.log("Unable to get websocket url!")
    };
    /*let rec onAnimationFrame = () => {
        updateFrame(self);
        requestAnimationFrame(onAnimationFrame);
      };
      requestAnimationFrame(onAnimationFrame);*/
    ReasonReact.NoUpdate;
  },
  render: ({state, send}) =>
    <section className="main"> <GameLayout dim0=3 dim1=4 boardCards=state.board /> </section>,
};
