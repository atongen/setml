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

(* encode from base-10 positive integer to base-3 list of 4 *)
let base3_list_of_int n =
  let rec aux n ac =
    if n < 0 || n > 80 then
      raise (Invalid_argument ("Only values 0 through 80 are supported"))
    else if n = 0 then
      if List.length ac = 0 then [0] else ac
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
  let v = base3_list_of_int n
          |> List.map attr_of_int
          |> Array.of_list
  in
  {
    num = v.(0);
    fill = v.(1);
    color = v.(2);
    shape = v.(3);
  }

let to_int x =
  float_of_int(attr_to_int x.num) ** 4. +.
  float_of_int(attr_to_int x.fill) ** 3. +.
  float_of_int(attr_to_int x.color) ** 2. +.
  float_of_int(attr_to_int x.shape)
  |> int_of_float

let to_string x =
  "{ n: " ^ (string_of_int (attr_to_int x.num)) ^
  ", f: " ^ (string_of_int (attr_to_int x.fill)) ^
  ", c: " ^ (string_of_int (attr_to_int x.color)) ^
  ", s: " ^ (string_of_int (attr_to_int x.shape)) ^ "}"

let compare x1 x2 = compare (to_int x1) (to_int x2)

let rec range i j = if i > j then [] else i :: (range (i+1) j)
let (--) i j = range i j
let deck = 0 -- 80 |> List.map of_int |> Array.of_list

let is_set c0 c1 c2 =
  List.for_all (fun f ->
      (f c0 == f c1 && f c1 == f c2) ||
      (f c0 != f c1 && f c1 != f c2 && f c0 != f c2)
    ) [num; fill; color; shape]

let is_set_idx idx0 idx1 idx2 =
  is_set deck.(idx0) deck.(idx1) deck.(idx2)

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
    shape = complete_attr c1.shape c1.shape;
  }

let combinations l k =
  let rec aux l acc k =
    match k with
    | 0 -> [[]]
    | _ ->
      match l with
      | [] -> acc
      | x :: xs ->
        let rec accmap acc f = function
          | [] -> acc
          | x :: xs -> accmap ((f x) :: acc) f xs
        in
        let newacc = accmap acc (fun z -> x :: z) (aux xs [] (k - 1))
        in aux xs newacc k
  in aux l [] k

let triples l =
  let s = List.sort_uniq compare l in
  let arrays = List.map Array.of_list (combinations s 3) in
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> if Array.length hd = 3 then
        aux ((hd.(0), hd.(1), hd.(2)) :: acc) tl
      else aux acc tl
  in aux [] arrays

let find_sets cards =
  let rec aux acc = function
    | [] -> acc
    | (c0, c1, c2) as hd :: tl ->
      if is_set c0 c1 c2 then
        aux (hd :: acc) tl
      else
        aux acc tl
  in aux [] (triples cards)

let find_sets_idx idxs =
  List.map of_int idxs |> find_sets

let count_sets cards = List.length (find_sets cards)

let count_sets_idx idxs = List.length (find_sets_idx idxs)

let exist_set cards =
  let rec aux = function
    | [] -> false
    | (c0, c1, c2) :: tl ->
      if is_set c0 c1 c2 then true
      else aux tl
  in aux (triples cards)

let exist_set_idx idxs = List.map of_int idxs |> exist_set
