let window = Webapi.Dom.window;

let devicePixelRatio = () : float => [%bs.raw {| window.devicePixelRatio || 1.0 |}];

let size_of_ratio = (size, ratio) => int_of_float(floor(float_of_int(size) *. ratio +. 0.5));

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

let make = (_children, ~dim0, ~dim1) => {
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
    if (screen.width >= screen.height) {
      /* landscape */
      let idealBoard = float_of_int(screen.height);
      let idealBlock = idealBoard /. float_of_int(rows);
      let idealSidebar = float_of_int(screen.width) -. idealBlock *. float_of_int(columns);
      let minSidebar = float_of_int(screen.width) *. sidebarMinRatio;
      let sidebar = int_of_float(ClientUtil.round(max(minSidebar, idealSidebar)));
      let board = screen.width - sidebar;
      <div>
        <Board top=0 bottom=screen.height left=0 right=board ratio=screen.ratio columns rows />
        <Sidebar top=0 bottom=screen.height left=board right=screen.width summary=true />
      </div>;
    } else {
      /* portrait */
      let idealBoard = float_of_int(screen.width);
      let idealBlock = idealBoard /. float_of_int(columns);
      let idealSidebar = float_of_int(screen.height) -. idealBlock *. float_of_int(rows);
      let minSidebar = float_of_int(screen.height) *. sidebarMinRatio;
      let sidebar = int_of_float(ClientUtil.round(max(minSidebar, idealSidebar)));
      let board = screen.height - sidebar;
      <div>
        <Board top=0 bottom=board left=0 right=screen.width ratio=screen.ratio columns rows />
        <Sidebar top=board bottom=screen.height left=0 right=screen.width summary=false />
      </div>;
    };
  },
};
