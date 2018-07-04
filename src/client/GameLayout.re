open Belt;

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

let handleResize = (evt, self) => self.ReasonReact.send(Resize);

let component = ReasonReact.reducerComponent("GameLayout");

let make = (_children, ~dim0, ~dim1, ~boardCards, ~sendMessage) => {
  ...component,
  reducer: (action, state) =>
    switch (action) {
    | Resize => ReasonReact.Update(getState(dim0, dim1))
    },
  initialState: () => getState(dim0, dim1),
  subscriptions: self => {
    let debouncedHandleResize = ClientUtil.debounceOne(100, self.handle(handleResize));
    [
      Sub(
        () => WindowRe.addEventListener("resize", debouncedHandleResize, window),
        () => WindowRe.removeEventListener("resize", debouncedHandleResize, window),
      ),
    ];
  },
  render: self => {
    let screen = self.state.screen;
    let columns = self.state.columns;
    let rows = self.state.rows;
    let sidebarMinRatio = 0.2;
    let (boardRect, sidebarRect) =
      if (screen.width >= screen.height) {
        /* landscape */
        let idealBoard = float_of_int(screen.height);
        let idealBlock = idealBoard /. float_of_int(rows);
        let idealSidebar = float_of_int(screen.width) -. idealBlock *. float_of_int(columns);
        let minSidebar = float_of_int(screen.width) *. sidebarMinRatio;
        let sidebar = Shared_util.roundi(max(minSidebar, idealSidebar));
        let board = screen.width - sidebar;
        (Rect.makei(0, 0, board, screen.height), Rect.makei(board, 0, screen.width - board, screen.height));
      } else {
        /* portrait */
        let idealBoard = float_of_int(screen.width);
        let idealBlock = idealBoard /. float_of_int(columns);
        let idealSidebar = float_of_int(screen.height) -. idealBlock *. float_of_int(rows);
        let minSidebar = float_of_int(screen.height) *. sidebarMinRatio;
        let sidebar = Shared_util.roundi(max(minSidebar, idealSidebar));
        let board = screen.height - sidebar;
        (Rect.makei(0, 0, screen.width, board), Rect.makei(0, board, screen.width, screen.height - board));
      };
    <div>
      <Board rect=boardRect ratio=screen.ratio columns rows boardCards sendMessage />
      <Sidebar rect=sidebarRect sendMessage />
    </div>;
  },
};
