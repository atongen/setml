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

let make =
    (
      _children,
      ~boardCards,
      ~players,
      ~game: Messages.game_update_data,
      ~sendMessage,
      ~notifications,
      ~setNotifications,
    ) => {
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
    let c = float_of_int(columns);
    let r = float_of_int(rows);
    let sidebarMinRatio = 0.2;
    let width = float_of_int(screen.width);
    let height = float_of_int(screen.height);
    let (boardRect, sidebarRect) =
      if (width >= height) {
        /* landscape */
        let idealBoard = height;
        let idealBlock = idealBoard /. r;
        let idealSidebar = width -. idealBlock *. c;
        let minSidebar = width *. sidebarMinRatio;
        let sidebar = max(minSidebar, idealSidebar);
        let board = width -. sidebar;
        (Rect.make(0.0, 0.0, board, height), Rect.make(board, 0.0, width -. board, height));
      } else {
        /* portrait */
        let idealBoard = width;
        let idealBlock = idealBoard /. c;
        let idealSidebar = height -. idealBlock *. r;
        let minSidebar = height *. sidebarMinRatio;
        let sidebar = max(minSidebar, idealSidebar);
        let board = height -. sidebar;
        (Rect.make(0.0, 0.0, width, board), Rect.make(0.0, board, width, height -. board));
      };
    let br = Rect.shrink(~i=5.0, boardRect);
    let sr = Rect.shrink(~i=5.0, sidebarRect);
    <MaterialUi.CssBaseline>
      <Board rect=br columns rows boardCards game sendMessage />
      <Sidebar rect=sr boardCards players game sendMessage notifications setNotifications />
    </MaterialUi.CssBaseline>;
  },
};
