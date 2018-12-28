open Belt;

open Messages;

open ClientMessages;

type action =
  | SendMessage(Messages.t)
  | ReceiveMessage(string)
  | OpenMessage
  | CloseMessage(string)
  | ErrorMessage(string)
  | Online
  | Offline

type state = {
  ws: ref(option(WebSockets.WebSocket.t)),
  boardCards: list(Messages.board_card_data),
  players: list(player_data),
  game: game_update_data,
  msgs: list(string),
  online: bool,
};

let replaceBoardCard =
    (board_card_data: board_card_data, board_cards: list(board_card_data))
    : list(board_card_data) => {
  let rec aux = (bcd: board_card_data, acc, l: list(board_card_data)) : list(board_card_data) =>
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

let replacePlayer = (player_data: player_data, players: list(player_data)) => {
  let rec aux = (pd: player_data, acc: list(player_data), l: list(player_data)) =>
    switch (l) {
    | [] => acc
    | [hd, ...tl] =>
      if (hd.player_id == pd.player_id) {
        aux(pd, [pd, ...acc], tl);
      } else {
        aux(pd, [hd, ...acc], tl);
      }
    };
  aux(player_data, [], List.reverse(players));
};

let updatePlayerName = (name_data: name_data, players: list(player_data)) => {
  let pdo = List.getBy(players, pd => pd.player_id == name_data.player_id);
  switch (pdo) {
  | Some(pd) =>
    let npd = {...pd, name: name_data.name};
    replacePlayer(npd, players);
  | None => players
  };
};

let updatePlayerScore = (score_data: score_data, players: list(player_data)) => {
  let pdo = List.getBy(players, pd => pd.player_id == score_data.player_id);
  switch (pdo) {
  | Some(pd) =>
    let npd = {...pd, score: score_data.score};
    replacePlayer(npd, players);
  | None => players
  };
};

let updatePlayerPresence = (presence_data: presence_data, players: list(player_data)) => {
  let pdo = List.getBy(players, pd => pd.player_id == presence_data.player_id);
  switch (pdo) {
  | Some(pd) =>
    let npd = {...pd, presence: presence_data.presence};
    replacePlayer(npd, players);
  | None => players
  };
};

let updatePlayerShuffles = (shuffle_data: shuffle_data, players: list(player_data)) => {
  let pdo = List.getBy(players, pd => pd.player_id == shuffle_data.player_id);
  switch (pdo) {
  | Some(pd) =>
    let npd = {...pd, shuffles: shuffle_data.shuffles};
    replacePlayer(npd, players);
  | None => players
  };
};

let moveDataToString = (theme, move_data) => {
  let s0 = Theme.card_to_string(~theme, move_data.card0.card);
  let s1 = Theme.card_to_string(~theme, move_data.card1.card);
  let s2 = Theme.card_to_string(~theme, move_data.card2.card);
  Printf.sprintf("(%s, %s, %s)", s0, s1, s2);
};

let msgIfNotCurrentPlayer = (player_id, msg) =>
  if (ClientUtil.is_current_player(player_id)) {
    [];
  } else {
    [msg];
  };

let log = msg => ReasonReact.SideEffects(_self => Js.log(msg));

let handleReceiveMessage = (state, msg) =>
  switch (msg) {
  | Server_game(d) =>
    ReasonReact.Update({
      ...state,
      boardCards: d.board_card_data,
      players: d.player_data,
      game: d.game_update_data,
      msgs: [],
    })
  | Server_player(d) => ReasonReact.Update({...state, players: replacePlayer(d, state.players), msgs: []})
  | Server_name(d) =>
    let old_name = ClientUtil.player_name(state.players, d.player_id);
    let new_players = updatePlayerName(d, state.players);
    let msgs = msgIfNotCurrentPlayer(d.player_id, old_name ++ " is now called " ++ d.name)
    ReasonReact.Update({...state, players: new_players, msgs});
  | Server_card(_) => log("Received unhandled Server_card message");
  | Server_board_card(d) =>
    ReasonReact.Update({...state, boardCards: replaceBoardCard(d, state.boardCards), msgs: []})
  | Server_game_update(d) => ReasonReact.Update({...state, game: d, msgs: []})
  | Server_score(_) => log("Received unhandled Server_score message");
  | Server_move(_) => log("Received unhandled Server_move message");
  | Server_presence(d) =>
    let action =
      if (d.presence) {
        "joined";
      } else {
        "left";
      };
    let msgs = msgIfNotCurrentPlayer(d.player_id, ClientUtil.player_name(state.players, d.player_id) ++ " " ++ action ++ "!");
    ReasonReact.Update({...state, players: updatePlayerPresence(d, state.players), msgs});
  | Server_move_info(d) =>
    let playerName = ClientUtil.player_name(state.players, d.score_data.player_id)
    Js.log(playerName ++ ": " ++ moveDataToString(state.game.theme, d.move_data));
    let msgs = msgIfNotCurrentPlayer(d.score_data.player_id, playerName ++ " scored!");
    ReasonReact.Update({
      ...state,
      players: updatePlayerScore(d.score_data, state.players),
      msgs,
    });
  | Server_shuffles(d) =>
    let msgs = msgIfNotCurrentPlayer(d.player_id, ClientUtil.player_name(state.players, d.player_id) ++ " shuffled!");
    ReasonReact.Update({
      ...state,
      players: updatePlayerShuffles(d, state.players),
      msgs,
    })
  | Client_move(_)
  | Client_shuffle(_)
  | Client_start_game(_)
  | Client_name(_)
  | Client_theme(_)
  | Client_ping(_) =>
    log("Client received a client message");
  };

let window = Webapi.Dom.window;

let wsMessage = (evt, self) => {
  let str = WebSockets.MessageEvent.stringData(evt);
  self.ReasonReact.send(ReceiveMessage(str));
};

let wsOpen = ((), self) => {
  self.ReasonReact.send(OpenMessage);
};

let wsClose = (evt, self) => {
  let str = WebSockets.CloseEvent.reason(evt);
  self.ReasonReact.send(CloseMessage(str));
};

let wsError = (msg, self) => {
  self.ReasonReact.send(ErrorMessage(msg));
};

let setupWebsocket = (ws, self) => {
  WebSockets.WebSocket.(
    ws
    |> on(Open(self.ReasonReact.handle(wsOpen)))
    |> on(Close(self.handle(wsClose)))
    |> on(Message(self.handle(wsMessage)))
    |> on(Error(self.handle(wsError)))
    |> ignore
  );
};

let connectWebsocket = self => {
  switch (ClientUtil.ws_url()) {
  | Some(ws_url) => {
    let ws = WebSockets.WebSocket.make(ws_url);
    self.ReasonReact.state.ws := Some(ws);
    setupWebsocket(ws, self)
  };
  | None => Js.log("Unable to get websocket url!")
  };
};

let handleOnline = (_evt, self) => self.ReasonReact.send(Online);

let handleOffline = (_evt, self) => self.ReasonReact.send(Offline);

let wsSendMessage = (msg, ws) => {
  switch (ws) {
  | {contents: Some(ws)} => {
      let str = ClientMessageConverter.to_json(msg);
      WebSockets.WebSocket.sendString(str, ws)
    }
  | _ => Js.log("Unable to send: No websocket connection!")
  };
};

let ping = self => wsSendMessage(ClientUtil.make_ping_msg(), self.ReasonReact.state.ws);

let component = ReasonReact.reducerComponent("GameTop");

let make = _children => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | ReceiveMessage(str) =>
      let msg = ClientMessageConverter.of_json(str);
      handleReceiveMessage(state, msg);
    | SendMessage(msg) => ReasonReact.SideEffects(self => wsSendMessage(msg, self.state.ws));
    | OpenMessage => ReasonReact.SideEffects(self => ping(self));
    | CloseMessage(msg) => log("Websocket connection closed: " ++ msg);
    | ErrorMessage(msg) => log("Websocket error: " ++ msg);
    | Online => ReasonReact.UpdateWithSideEffects({...state, online: true}, self => connectWebsocket(self));
    | Offline => ReasonReact.Update({...state, ws: ref(None), online: false});
    },
  initialState: () => {
    ws: ref(None),
    boardCards: [],
    players: [],
    game: make_game_update_data(0, "new", "classic", 3, 4, None),
    msgs: [],
    online: true,
  },
  didMount: self => {
    connectWebsocket(self);
    let online = self.handle(handleOnline);
    let offline = self.handle(handleOffline);
    WindowRe.addEventListener("online", online, window);
    WindowRe.addEventListener("offline", offline, window);
    self.onUnmount(() => {
      WindowRe.removeEventListener("online", online, window);
      WindowRe.removeEventListener("offline", offline, window);
    });
  },
  render: self => {
    let sendMessage = msg => self.ReasonReact.send(SendMessage(msg));
    <section className="main">
      <GameLayout boardCards=self.state.boardCards players=self.state.players game=self.state.game sendMessage />
      <ConsecutiveSnackbars messages=self.state.msgs />
    </section>;
  },
};
