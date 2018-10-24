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

let of_int n =
  let v = Base_conv.base_list_of_int ~base:3 ~size:4 n |> Array.of_list in
  make v.(0) v.(1) v.(2) v.(3)

let of_int_opt n =
  if n >= 0 && n < 81 then Some (of_int n)
  else None

let of_int_list l = List.map of_int_opt l

let to_int x =
  (attr_to_int x.num) * 27 +
  (attr_to_int x.fill) * 9 +
  (attr_to_int x.color) * 3 +
  (attr_to_int x.shape)

let to_int_opt x =
  match x with
  | Some c -> to_int c
  | None -> 81

let to_int_list l = List.map to_int_opt l

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
  let (--^) i j = range i (j-1)
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

let find_sets cards =
  let tg = Combinatorics.triple_generator ~comp:compare cards in
  let rec aux acc = function
    | Some (triple) -> if is_triple_set triple then
        aux (triple :: acc) (tg ())
      else
        aux acc (tg ())
    | None -> acc
  in
  aux [] (tg ())

let find_non_sets cards =
  let tg = Combinatorics.triple_generator ~comp:compare cards in
  let rec aux acc = function
    | Some (triple) -> if not (is_triple_set triple) then
        aux (triple :: acc) (tg ())
      else
        aux acc (tg ())
    | None -> acc
  in
  aux [] (tg ())

let count_sets cards =
  let tg = Combinatorics.triple_generator ~comp:compare cards in
  let rec aux acc = function
    | Some (triple) -> if is_triple_set triple then
        aux (acc + 1) (tg ())
      else
        aux acc (tg ())
    | None -> acc
  in
  aux 0 (tg ())

let count_non_sets cards =
  let l = Combinatorics.choose (List.length cards) 3 in
  l - (count_sets cards)

let exists_set cards =
  let tg = Combinatorics.triple_generator ~comp:compare cards in
  let rec aux = function
    | Some (triple) -> if is_triple_set triple then true
      else aux (tg ())
    | None -> false
  in aux (tg ())

let exists_non_set cards =
  let tg = Combinatorics.triple_generator ~comp:compare cards in
  let rec aux = function
    | Some (triple) -> if not (is_triple_set triple) then true
      else aux (tg ())
    | None -> false
  in aux (tg ())
