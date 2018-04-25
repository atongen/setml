type attr =
  | AttrZero
  | AttrOne
  | AttrTwo

type t = {
  num: attr;
  fill: attr;
  color: attr;
  shape: attr;
}

let num x = x.num
let fill x = x.fill
let color x  = x.color
let shape x = x.shape

let attr_to_int = function
  | AttrZero -> 0
  | AttrOne -> 1
  | AttrTwo -> 2

let attr_of_int = function
  | 0 -> AttrZero
  | 1 -> AttrOne
  | 2 -> AttrTwo
  | _ -> raise (Invalid_argument ("Only 0, 1, or 2 are valid"))

let make n f c s =
  {
    num = attr_of_int n;
    fill = attr_of_int f;
    color = attr_of_int c;
    shape = attr_of_int s;
  }

(* encode from base-10 positive integer to base-3 list of 4 *)
let base3_list_of_int n =
  let rec aux n ac =
    if n < 0 || n > 80 then
      raise (Invalid_argument ("Only values 0 through 80 are supported"))
    else if n = 0 then ac
    else
      let next = n / 3 in
      let remainder = n mod 3 in
      aux next (remainder :: ac)
  in
  let rec fill l n v =
    if List.length l < n then
      fill (v :: l) n v
    else l
  in
  fill (aux n []) 4 0

let of_int n =
  let v = base3_list_of_int n |> Array.of_list in
  make v.(0) v.(1) v.(2) v.(3)

let to_int x =
  (attr_to_int x.num) * 27 +
  (attr_to_int x.fill) * 9 +
  (attr_to_int x.color) * 3 +
  (attr_to_int x.shape)

let to_string x =
  Printf.sprintf "{ n: %d, f: %d, c: %d, s: %d }"
    (attr_to_int x.num)
    (attr_to_int x.fill)
    (attr_to_int x.color)
    (attr_to_int x.shape)

let compare x0 x1 = compare (to_int x0) (to_int x1)

let equal x0 x1 =
  List.for_all (fun f -> f x0 == f x1) [num; fill; color; shape]

let rec range i j = if i > j then [] else of_int i :: (range (i+1) j)

module Infix = struct
  let (--) i j = range i j
end
include Infix

let deck () = 0 -- 80 |> Array.of_list

let is_set c0 c1 c2 =
  List.for_all (fun f ->
      (f c0 == f c1 && f c1 == f c2) ||
      (f c0 != f c1 && f c1 != f c2 && f c0 != f c2)
    ) [num; fill; color; shape]

let is_triple_set (c0, c1, c2) = is_set c0 c1 c2

let complete_attr a0 a1 =
  match (a0, a1) with
  | (AttrZero, AttrZero) | (AttrOne, AttrOne) | (AttrTwo, AttrTwo) -> a0
  | (AttrOne, AttrTwo) | (AttrTwo, AttrOne) -> AttrZero
  | (AttrZero, AttrTwo) | (AttrTwo, AttrZero) -> AttrOne
  | (AttrZero, AttrOne) | (AttrOne, AttrZero) -> AttrTwo

let complete c0 c1 =
  {
    num = complete_attr c0.num c1.num;
    fill = complete_attr c0.fill c1.fill;
    color = complete_attr c0.color c1.color;
    shape = complete_attr c0.shape c1.shape;
  }

let triples cards =
  let s = List.sort_uniq compare cards in
  let arrays = List.map Array.of_list (Combinatorics.comb0 s 3) in
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> if Array.length hd = 3 then
        aux ((hd.(0), hd.(1), hd.(2)) :: acc) tl
      else
        raise (Invalid_argument("Triple combination with length other than 3"))
  in aux [] arrays

let triple_generator cards =
  let s = List.sort_uniq compare cards in
  let gen = Combinatorics.comb_generator s 3 in
  let tg () =
    match gen () with
    | Some(l) ->
      if List.length l = 3 then
        let a = Array.of_list l in
        Some(a.(0), a.(1), a.(2))
      else
        raise (Invalid_argument("Triple combination with length other than 3"))
    | None -> None
  in
  tg

let find_sets cards =
  let tg = triple_generator cards in
  let rec aux acc = function
    | Some (triple) -> if is_triple_set triple then
        aux (triple :: acc) (tg ())
      else
        aux acc (tg ())
    | None -> acc
  in
  aux [] (tg ())

let find_sets_idx idxs = List.map of_int idxs |> find_sets

let find_non_sets cards =
  let tg = triple_generator cards in
  let rec aux acc = function
    | Some (triple) -> if not (is_triple_set triple) then
        aux (triple :: acc) (tg ())
      else
        aux acc (tg ())
    | None -> acc
  in
  aux [] (tg ())

let find_non_sets_idx idxs = List.map of_int idxs |> find_non_sets

let count_sets cards =
  let tg = triple_generator cards in
  let rec aux acc = function
    | Some (triple) -> if is_triple_set triple then
        aux (acc + 1) (tg ())
      else
        aux acc (tg ())
    | None -> acc
  in
  aux 0 (tg ())

let count_sets_idx idxs = List.map of_int idxs |> count_sets

let count_non_sets cards =
  let l = Combinatorics.choose (List.length cards) 3 in
  l - (count_sets cards)

let count_non_sets_idx idxs = List.map of_int idxs |> count_non_sets

let exists_set cards =
  let tg = triple_generator cards in
  let rec aux = function
    | Some (triple) -> if is_triple_set triple then true
      else aux (tg ())
    | None -> false
  in aux (tg ())

let exists_set_idx idxs = List.map of_int idxs |> exists_set

let exists_non_set cards =
  let tg = triple_generator cards in
  let rec aux = function
    | Some (triple) -> if not (is_triple_set triple) then true
      else aux (tg ())
    | None -> false
  in aux (tg ())

let exists_non_set_idx idxs = List.map of_int idxs |> exists_non_set

let find_idx x l =
  let rec aux x c = function
    | [] -> None
    | hd::tl -> if (equal hd x) then Some(c) else aux x (c+1) tl
  in aux x 0 l

let triple_with_indexes cards (c0, c1, c2) =
  let oc0 = find_idx c0 cards in
  let oc1 = find_idx c1 cards in
  let oc2 = find_idx c2 cards in
  match (oc0, oc1, oc2) with
  | (Some(idx0), Some(idx1), Some(idx2)) ->
    Some((
        (idx0, c0),
        (idx1, c1),
        (idx2, c2)
      ))
  | _ -> None

let next_set_and_indexes cards =
  let tg = triple_generator cards in
  let rec aux = function
    | Some (triple) -> if is_triple_set triple then
        triple_with_indexes cards triple
      else aux (tg ())
    | None -> None
  in aux (tg ())
