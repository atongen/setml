open Belt;

let map_quad = (f, (a, b, c, d)) => (f(a), f(b), f(c), f(d));

let debounceOne = (delay, f) => {
  let timeout = ref(None);
  v => {
    let later = () => {
      timeout := None;
      f(v);
    };
    switch (timeout) {
    | {contents: None} => ()
    | {contents: Some(timeoutId)} => Js.Global.clearTimeout(timeoutId)
    };
    timeout := Some(Js.Global.setTimeout(later, delay));
  };
};

let meta_content_opt = name => {
  let rec aux = c =>
    switch (c) {
    | [] => None
    | [hd, ...tl] =>
      let el_name = Webapi.Dom.Element.getAttribute("name", hd);
      switch (el_name) {
      | None => aux(tl)
      | Some(n) =>
        if (n == name) {
          Webapi.Dom.Element.getAttribute("content", hd);
        } else {
          aux(tl);
        }
      };
    };
  DocumentRe.getElementsByTagName("meta", Webapi.Dom.document)
  |> Webapi.Dom.HtmlCollection.toArray
  |> List.fromArray
  |> aux;
};

let meta_content = name =>
  switch (meta_content_opt(name)) {
  | Some(c) => c
  | None => ""
  };

let make_move_msg = (cd0, cd1, cd2) => {
  let token = meta_content("token");
  Messages.Client_move((token, {card0: cd0, card1: cd1, card2: cd2}));
};

let make_shuffle_msg = () => {
  let token = meta_content("token");
  Messages.Client_shuffle(token);
};

let make_start_game_msg = () => {
  let token = meta_content("token");
  Messages.Client_start_game(token);
};

let make_name_msg = name => {
  let token = meta_content("token");
  Messages.Client_name((token, name));
};

let make_theme_msg = theme => {
  let token = meta_content("token");
  Messages.Client_theme((token, theme));
};

[@bs.get] external location : Dom.window => Dom.location = "";

[@bs.get] external pathname : Dom.location => string = "";

[@bs.get] external protocol : Dom.location => string = "";

[@bs.get] external host : Dom.location => string = "";

let path_parts = () =>
  switch ([%external window]) {
  | None => Array.make(0, "")
  | Some((window: Dom.window)) =>
    switch (window |> location |> pathname) {
    | ""
    | "/" => Array.make(0, "")
    | raw =>
      /* remove the preceeding /, which every pathname seems to have */
      let raw = Js.String.sliceToEnd(~from=1, raw);
      /* remove the trailing /, which some pathnames might have. Ugh */
      let raw =
        switch (Js.String.get(raw, Js.String.length(raw) - 1)) {
        | "/" => Js.String.slice(~from=0, ~to_=-1, raw)
        | _ => raw
        };
      raw |> Js.String.split("/");
    }
  };

let game_id = () => {
  let parts = path_parts();
  if (Array.length(parts) == 2 && parts[0] == Some("games")) {
    parts[1];
  } else {
    None;
  };
};

let with_window = f =>
  switch ([%external window]) {
  | None => None
  | Some((window: Dom.window)) => f(window)
  };

let hostname = () =>
  with_window(window =>
    switch (window |> location |> host) {
    | "" => None
    | host => Some(host)
    }
  );

let my_protocol = () =>
  with_window(window =>
    switch (window |> location |> protocol) {
    | "" => None
    | protocol => Some(protocol)
    }
  );

let ws_url = () =>
  switch (my_protocol(), hostname(), game_id()) {
  | (Some(p), Some(h), Some(gid)) =>
    let ws_protocol = p == "https:" ? "wss:" : "ws:";
    Some(ws_protocol ++ "//" ++ h ++ "/games/" ++ gid ++ "/ws");
  | _ => None
  };

let game_url = () =>
  switch (my_protocol(), hostname(), game_id()) {
  | (Some(p), Some(h), Some(gid)) => Some(p ++ "//" ++ h ++ "/games/" ++ gid)
  | _ => None
  };

let get_player_name = (player_data: Messages.player_data) => {
  let name = player_data.name;
  if (String.trim(name) == "") {
    "Player " ++ string_of_int(player_data.player_id);
  } else {
    name;
  };
};

let player_name = (players: list(Messages.player_data), player_id) => {
  let maybePlayer = List.getBy(players, player => player.player_id == player_id);
  switch (maybePlayer) {
  | Some(player) => get_player_name(player)
  | None => "Player " ++ string_of_int(player_id)
  };
};

let current_player_id = () =>
  switch (meta_content_opt("player_id")) {
  | Some(pid) => Some(int_of_string(pid))
  | None => None
  };

let is_current_player = id =>
  switch (current_player_id()) {
  | Some(pid) => pid == id
  | None => false
  };

let current_player_name = (players: list(Messages.player_data)) =>
  switch (current_player_id()) {
  | Some(pid) => Some(player_name(players, pid))
  | None => None
  };

let closeToSquare = (~d0=5.0, ~d1=6.0, width, height) => {
  let ratio = width /. height;
  ratio >= d0 /. d1 && ratio <= d1 /. d0;
};

let calculateBorder = (width, height) => min(width, height) /. 75.;

let rectToStyle = rect =>
  ReactDOMRe.Style.make(
    ~top=Shared_util.roundis(Rect.top(rect)) ++ "px",
    ~bottom=Shared_util.roundis(Rect.bottom(rect)) ++ "px",
    ~left=Shared_util.roundis(Rect.left(rect)) ++ "px",
    ~right=Shared_util.roundis(Rect.right(rect)) ++ "px",
    ~width=Shared_util.roundis(rect.w) ++ "px",
    ~height=Shared_util.roundis(rect.h) ++ "px",
  );

let distanceOfTimeInWords = (fromTime, toTime) => {
  let distanceInSeconds = toTime -. fromTime;
  let rel =
    if (distanceInSeconds > 0.0) {
      "from now";
    } else {
      "ago";
    };
  let dis = abs_float(distanceInSeconds);
  let dim = dis /. 60.0;
  let r = Shared_util.roundi;
  let s = Printf.sprintf;
  let dot =
    if (dim < 1.0) {
      if (dis < 5.0) {
        "less than 5 seconds";
      } else if (5.0 <= dis && dis < 10.0) {
        "less than 10 seconds";
      } else if (10.0 <= dis && dis < 20.0) {
        "less than 20 seconds";
      } else if (20.0 <= dis && dis < 40.0) {
        "half a minute";
      } else {
        "less than a minute";
      };
    } else if (2.0 <= dim && dim < 45.0) {
      s("%d minutes", r(dim));
    } else if (45.0 <= dim && dim < 90.0) {
      "about 1 hour";
    } else if (90.0 <= dim && dim < 1440.0) {
      s("about %d hours", r(dim /. 60.0));
    } else if (1440.0 <= dim && dim < 2520.0) {
      "about 1 day";
    } else if (2520.0 <= dim && dim < 43200.0) {
      s("about %d days", r(dim /. 1440.0));
    } else if (43200.0 <= dim && dim < 86400.0) {
      s("about %d months", r(dim /. 43200.0));
    } else if (86400.0 <= dim && dim < 525600.0) {
      s("%d months", r(dim /. 43200.0));
    } else {
      let diy = dim /. 525949.2;
      let rounded = Shared_util.roundi(diy);
      let rem = diy -. float_of_int(rounded);
      if (rem < 0.25) {
        s("over %d years", rounded);
      } else if (0.25 <= rem && rem < 0.75) {
        s("about %d and one half years", rounded);
      } else {
        s("under %d years", rounded);
      };
    };
  s("%s %s", dot, rel);
};

let sortPlayers = (players: list(Messages.player_data)) => {
  let comparePlayers = (p0: Messages.player_data, p1: Messages.player_data) =>
    if (p0.score == p1.score) {
      compare(p0.shuffles, p1.shuffles);
    } else {
      compare(p1.score, p0.score);
    };
  List.sort(players, comparePlayers);
};
