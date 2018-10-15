let window = Webapi.Dom.window;

let devicePixelRatio = () : float => [%bs.raw {| window.devicePixelRatio || 1.0 |}];

type action =
  | Resize;

type screen = {
  width: int,
  height: int,
  ratio: float,
};

type state = {
  columns: int,
  rows: int,
  screen,
};


let getScreen = () => {
  width: Webapi.Dom.Window.innerWidth(window),
  height: Webapi.Dom.Window.innerHeight(window),
  ratio: devicePixelRatio(),
};

let getState = (dim0, dim1) => {
  let screen = getScreen();
  if (screen.width >= screen.height) {
    let (columns, rows) = dim0 >= dim1 ? (dim0, dim1) : (dim1, dim0);
    {columns, rows, screen};
  } else {
    let (columns, rows) = dim0 <= dim1 ? (dim0, dim1) : (dim1, dim0);
    {columns, rows, screen};
  };
};

let handleResize = (_evt, self) => self.ReasonReact.send(Resize);

let component = ReasonReact.reducerComponent("GameLayout");

let make = (_children, ~boardCards, ~players, ~game: Messages.game_update_data, ~previousMove, ~sendMessage) => {
  ...component,
  reducer: (action, _state) =>
    switch (action) {
    | Resize => ReasonReact.Update(getState(game.dim0, game.dim1))
    },
  initialState: () => getState(game.dim0, game.dim1),
  didMount: self => {
    let debouncedHandleResize = ClientUtil.debounceOne(100, self.handle(handleResize));
    WindowRe.addEventListener("resize", debouncedHandleResize, window);
    self.onUnmount(() => WindowRe.removeEventListener("resize", debouncedHandleResize, window));
  },
  render: self => {
    let screen = self.state.screen;
    let columns = self.state.columns;
    let rows = self.state.rows;
    let sidebarMinRatio = 0.2;
    let appBarOffset = 64;
    let usableHeight = screen.height - appBarOffset;
    let (boardRect, sidebarRect) =
      if (screen.width >= usableHeight) {
        /* landscape */
        let idealBoard = float_of_int(usableHeight);
        let idealBlock = idealBoard /. float_of_int(rows);
        let idealSidebar = float_of_int(screen.width) -. idealBlock *. float_of_int(columns);
        let minSidebar = float_of_int(screen.width) *. sidebarMinRatio;
        let sidebar = Shared_util.roundi(max(minSidebar, idealSidebar));
        let board = screen.width - sidebar;
        (Rect.makei(0, appBarOffset, board, usableHeight),
         Rect.makei(board, appBarOffset, screen.width - board, usableHeight));
      } else {
        /* portrait */
        let idealBoard = float_of_int(screen.width);
        let idealBlock = idealBoard /. float_of_int(columns);
        let idealSidebar = float_of_int(usableHeight) -. idealBlock *. float_of_int(rows);
        let minSidebar = float_of_int(usableHeight) *. sidebarMinRatio;
        let sidebar = Shared_util.roundi(max(minSidebar, idealSidebar));
        let board = usableHeight - sidebar;
        (Rect.makei(0, appBarOffset, screen.width, board),
         Rect.makei(0, board + appBarOffset, screen.width, usableHeight - board));
      };
    <div>
      <Board rect=boardRect ratio=screen.ratio columns rows boardCards game sendMessage />
      <Sidebar rect=sidebarRect boardCards players game previousMove sendMessage />
    </div>;
  },
};
