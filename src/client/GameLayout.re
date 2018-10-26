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

let make = (_children, ~boardCards, ~players, ~game: Messages.game_update_data, ~sendMessage) => {
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
    let appBarOffset = 64.0;
    let boardOffsetY = appBarOffset;
    let width = float_of_int(screen.width);
    let height = float_of_int(screen.height);
    let usableHeight = height -. appBarOffset;
    let (boardRect, sidebarRect, boardOffsetX) =
      if (width >= usableHeight) {
        /* landscape */
        let idealBoard = usableHeight;
        let idealBlock = idealBoard /. float_of_int(rows);
        let idealSidebar = width -. idealBlock *. float_of_int(columns);
        let minSidebar = width *. sidebarMinRatio;
        let sidebar = max(minSidebar, idealSidebar);
        let board = width -. sidebar;
        (
          Rect.make(0.0, appBarOffset, board, usableHeight),
          Rect.make(board, appBarOffset, width -. board, usableHeight),
          0.0,
        );
      } else {
        /* portrait */
        let idealBoard = width;
        let idealBlock = idealBoard /. float_of_int(columns);
        let idealSidebar = usableHeight -. idealBlock *. float_of_int(rows);
        let minSidebar = usableHeight *. sidebarMinRatio;
        let sidebar = max(minSidebar, idealSidebar);
        let board = usableHeight -. sidebar;
        (
          Rect.make(0.0, appBarOffset, width, board),
          Rect.make(0.0, board +. appBarOffset, width, usableHeight -. board),
          0.0,
        );
      };
    <div>
      <Board rect=boardRect ratio=screen.ratio columns rows boardCards game sendMessage boardOffsetX boardOffsetY />
      <Sidebar rect=sidebarRect boardCards players game sendMessage />
    </div>;
  },
};
