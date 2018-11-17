open Belt;

open Messages;

open ClientMessages;

type action =
  | SendMessage(Messages.t)
  | ReceiveMessage(string)

type state = {
  ws: ref(option(WebSockets.WebSocket.t)),
  boardCards: list(Messages.board_card_data),
  players: list(player_data),
  game: game_update_data,
  msgs: list(string),
};

let receiveMessage = (evt, self) => {
  let str = WebSockets.MessageEvent.stringData(evt);
  self.ReasonReact.send(ReceiveMessage(str));
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
  Printf.sprintf("Previous move: (%s, %s, %s)", s0, s1, s2);
};

let msgIfNotCurrentPlayer = (player_id, msg) =>
  if (ClientUtil.is_current_player(player_id)) {
    [];
  } else {
    [msg];
  };

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
  | Server_card(_) =>
    Js.log("Received unhandled Server_card message");
    ReasonReact.NoUpdate;
  | Server_board_card(d) =>
    ReasonReact.Update({...state, boardCards: replaceBoardCard(d, state.boardCards), msgs: []})
  | Server_game_update(d) => ReasonReact.Update({...state, game: d, msgs: []})
  | Server_score(_) =>
    Js.log("Received unhandled Server_score message");
    ReasonReact.NoUpdate;
  | Server_move(_) =>
    Js.log("Received unhandled Server_move message");
    ReasonReact.NoUpdate;
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
    Js.log(moveDataToString(state.game.theme, d.move_data));
    let msgs = msgIfNotCurrentPlayer(d.score_data.player_id, ClientUtil.player_name(state.players, d.score_data.player_id) ++ " scored!");
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
  | Client_theme(_) =>
    Js.log("Client received a client message");
    ReasonReact.NoUpdate;
  };

let component = ReasonReact.reducerComponent("Game");

[%mui.withStyles
  "StyledAppBar"({
    root: ReactDOMRe.Style.make(~flexGrow="1", ()),
    grow: ReactDOMRe.Style.make(~flexGrow="1", ()),
    menuButton: ReactDOMRe.Style.make(~marginLeft="-12", ~marginRight="20", ()),
  })
];

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
  initialState: () => {
    ws: ref(None),
    boardCards: [],
    players: [],
    game: make_game_update_data(0, "new", "classic", 3, 4),
    msgs: [],
  },
  didMount: self => {
    switch (ClientUtil.ws_url()) {
    | Some(ws_url) =>
      let ws = WebSockets.WebSocket.make(ws_url);
      self.state.ws := Some(ws);
      WebSockets.WebSocket.(ws |> on(Message(self.handle(receiveMessage))) |> ignore);
    | None => Js.log("Unable to get websocket url!")
    };
    ();
  },
  render: self => {
    let sendMessage = msg => self.ReasonReact.send(SendMessage(msg));
    <section className="main">
      <GameLayout boardCards=self.state.boardCards players=self.state.players game=self.state.game sendMessage />
      <ConsecutiveSnackbars messages=self.state.msgs />
    </section>;
  },
};
