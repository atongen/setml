open Belt;

module RectComparator =
  Belt.Id.MakeComparable(
    {
      type t = Rect.t;
      let cmp = (a: t, b: t) => Rect.compare(a, b);
    },
  );

type t('a) = Map.t(Rect.t, 'a, RectComparator.identity);

let find = (grid, (x, y)) => {
  let search = Map.findFirstBy(grid, (k, _v) => Rect.isWithin(k, (x, y)));
  switch (search) {
  | Some((_k, v)) => Some(v)
  | None => None
  };
};

let make = (columns, rows, size, values) : t('a) => {
  let m = Belt.Map.make(~id=(module RectComparator));
  for (i in 0 to rows - 1) {
    for (j in 0 to columns - 1) {
      let idx = i * columns + j;
      switch (values[idx]) {
      | Some(v) =>
        let rect = Rect.make(float_of_int(j) *. size, float_of_int(i) *. size, size, size);
        Map.set(m, rect, v) |> ignore;
      | None => ()
      };
    };
  };
  m;
};
