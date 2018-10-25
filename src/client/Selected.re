open Belt;

module BoardCardDataComparator =
  Belt.Id.MakeComparable(
    {
      type t = Messages.board_card_data;
      let cmp = (a: t, b: t) => Pervasives.compare(a, b);
    },
  );

type t = Set.t(Messages.board_card_data, BoardCardDataComparator.identity);

let empty = Set.make(~id=(module BoardCardDataComparator));

let is_empty = s => Set.isEmpty(s);

let of_list = l => Set.fromArray(List.toArray(l), (module BoardCardDataComparator));

let to_list = s => Set.toList(s);

let merge_many = (s, a) => Set.mergeMany(s, a);

let intersect = (s0, s1) => Set.intersect(s0, s1);

let union = (s0, s1) => Set.union(s0, s1);

let diff = (s0, s1) => Set.diff(s0, s1);

let symmetric_diff = (s0, s1) => {
  let all = union(s0, s1);
  let common = intersect(s0, s1);
  diff(all, common);
};

let is_symmetric_diff = (s0, s1) => ! is_empty(symmetric_diff(s0, s1));

let to_string = selected =>
  "Selected: (" ++ String.concat(",", List.map(to_list(selected), Messages.board_card_data_to_string)) ++ ")";

let has = (selected, bcd) => Set.has(selected, bcd);

let add = (selected, bcd) => Set.add(selected, bcd);

let remove = (selected, bcd) => Set.remove(selected, bcd);

let size = selected => Set.size(selected);
