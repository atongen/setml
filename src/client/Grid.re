open Belt;

module RectComparator =
  Belt.Id.MakeComparable(
    {
      type t = Rect.t;
      let cmp = (a: t, b: t) => Rect.compare(a, b);
    },
  );

type t('a) = {
  marginX: float,
  marginY: float,
  border: float,
  width: float,
  height: float,
  columns: int,
  rows: int,
  blockSize: float,
  values: array('a),
  blocks: Map.t(Rect.t, int, RectComparator.identity),
};

let makeRect = (grid, x0, y0) => Rect.make(x0, y0, grid.width -. 2. *. x0, grid.height -. 2. *. y0);

let outerRect = grid => makeRect(grid, 0.0, 0.0);

let paddedRect = grid => makeRect(grid, grid.marginX, grid.marginY);

let innerRect = grid => {
  let x0 = grid.marginX +. grid.border;
  let y0 = grid.marginY +. grid.border;
  makeRect(grid, x0, y0);
};

let findByValue = (grid: t('a), f) : option('a) => {
  let rec aux = (a, i) =>
    switch (a[i]) {
    | Some(v) =>
      if (f(v)) {
        Some(v);
      } else {
        aux(a, i + 1);
      }
    | None => None
    };
  aux(grid.values, 0);
};

let findByPoint = (grid: t('a), (x, y)) => {
  let search = Map.findFirstBy(grid.blocks, (k, _v) => Rect.isWithin(k, (x, y)));
  switch (search) {
  | Some((_k, v)) => grid.values[v]
  | None => None
  };
};

let findByIdx = (grid: t('a), idx) => grid.values[idx];

let findKeyByIdx = (grid: t('a), idx) => {
  let search = Map.findFirstBy(grid.blocks, (_k, v) => v == idx);
  switch (search) {
  | Some((k, _v)) => Some(k)
  | None => None
  };
};

let forEachWithIndex = (grid: t('a), f) => Map.forEach(grid.blocks, (k, v) => f(k, grid.values[v], v));

let calculateUsableSize = (~border=0.0, ~width, ~height, ~columns, ~rows) => {
  let cp = float_of_int(columns + 1);
  let rp = float_of_int(rows + 1);
  (width -. cp *. border, height -. rp *. border);
};

let calculateBlockSize = (~width, ~height, ~columns, ~rows) => {
  let c = float_of_int(columns);
  let r = float_of_int(rows);
  let idealWidth = width /. c;
  let idealHeight = height /. r;
  if (idealWidth >= idealHeight) {
    let marginX = (width -. idealHeight *. c) /. 2.;
    (idealHeight, marginX, 0.);
  } else {
    let marginY = (height -. idealWidth *. r) /. 2.;
    (idealWidth, 0., marginY);
  };
};

let make = (~border=0.0, ~width, ~height, ~columns, ~rows, values) : t('a) => {
  let (usableWidth, usableHeight) = calculateUsableSize(~border, ~width, ~height, ~columns, ~rows);
  let (blockSize, marginX, marginY) = calculateBlockSize(~width=usableWidth, ~height=usableHeight, ~columns, ~rows);
  let rec aux = (m, n, acc) => {
    let i = m / columns;
    let j = m mod columns;
    let ii = float_of_int(i);
    let jj = float_of_int(j);
    let idx = i * columns + j;
    let bx = jj *. (blockSize +. border);
    let by = ii *. (blockSize +. border);
    let rect = Rect.make(bx +. border +. marginX, by +. border +. marginY, blockSize, blockSize);
    let result = [(rect, idx), ...acc];
    if (m < n - 1) {
      aux(m + 1, n, result);
    } else {
      result;
    };
  };
  let assoc = List.toArray(List.reverse(aux(0, columns * rows, [])));
  let blocks = Map.fromArray(assoc, ~id=(module RectComparator));
  {marginX, marginY, border, width, height, columns, rows, blockSize, values, blocks};
};
