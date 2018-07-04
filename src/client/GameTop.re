open Belt;

open Messages;

open ClientMessages;

type action =
  | SendMessage(Messages.t)
  | ReceiveMessage(string);

type state = {
  ws: ref(option(WebSockets.WebSocket.t)),
  board: list(Messages.board_card_data),
  players: list(player_data),
};

let receiveMessage = (evt, self) => {
  let str = WebSockets.MessageEvent.stringData(evt);
  self.ReasonReact.send(ReceiveMessage(str));
};

let replaceBoardCard =
    (board_card_data: Messages.board_card_data, board_cards: list(Messages.board_card_data))
    : list(Messages.board_card_data) => {
  let rec aux =
          (bcd: Messages.board_card_data, acc, l: list(Messages.board_card_data))
          : list(Messages.board_card_data) =>
    switch (l) {
    | [] => acc
    | [hd, ...tl] =>
      if (hd.idx == bcd.idx) {
        aux(bcd, [bcd, ...acc], tl);
      } else {
        aux(bcd, [hd, ...acc], tl);
      }
    };
  aux(board_card_data, [], List.reverse(board_cards));
};

let handleReceiveMessage = (state, msg) =>
  switch (msg) {
  | Server_game(d) => ReasonReact.Update({...state, board: d.board_card_data, players: d.player_data})
  | Server_player(d) => ReasonReact.NoUpdate
  | Server_name(d) => ReasonReact.NoUpdate
  | Server_card(d) => ReasonReact.NoUpdate
  | Server_board_card(d) => ReasonReact.Update({...state, board: replaceBoardCard(d, state.board)})
  | Server_game_update(d) => ReasonReact.NoUpdate
  | Server_score(d) => ReasonReact.NoUpdate
  | Server_move(d) => ReasonReact.NoUpdate
  | Server_presence(d) => ReasonReact.NoUpdate
  | Server_move_info(d) => ReasonReact.NoUpdate
  | Server_shuffles(d) => ReasonReact.NoUpdate
  | Client_move(_)
  | Client_shuffle(_) =>
    Js.log("Client received a client message");
    ReasonReact.NoUpdate;
  };

let component = ReasonReact.reducerComponent("Game");

let make = _children => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | ReceiveMessage(str) =>
      let msg = ClientMessageConverter.of_json(str);
      Js.log(Messages.to_string(msg));
      handleReceiveMessage(state, msg);
    | SendMessage(msg) =>
      let str = ClientMessageConverter.to_json(msg);
      switch (state.ws) {
      | {contents: Some(ws)} => WebSockets.WebSocket.sendString(str, ws)
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
      WebSockets.WebSocket.(ws |> on(Message(self.handle(receiveMessage))) |> ignore);
    | None => Js.log("Unable to get websocket url!")
    };
    ReasonReact.NoUpdate;
  },
  render: self => {
    let sendMessage = msg => self.ReasonReact.send(SendMessage(msg));
    <section className="main"> <GameLayout dim0=3 dim1=4 boardCards=self.state.board sendMessage /> </section>;
  },
};
