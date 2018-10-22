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

let to_list = s => Set.toList(s);

let to_string = selected =>
    "Selected: ("
    ++ String.concat(",", List.map(to_list(selected), Messages.board_card_data_to_string))
    ++ ")";

let has = (selected, bcd) => Set.has(selected, bcd);

let add = (selected, bcd) => Set.add(selected, bcd);

let remove = (selected, bcd) => Set.remove(selected, bcd);

let size = selected => Set.size(selected);
