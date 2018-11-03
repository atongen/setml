type attr = | AttrZero | AttrOne | AttrTwo

let attr_to_int = function
    | AttrZero -> 0
    | AttrOne -> 1
    | AttrTwo -> 2

let int_to_attr = function
    | 0 -> AttrZero
    | 1 -> AttrOne
    | 2 -> AttrTwo
    | _ -> raise (Invalid_argument ("Only 0, 1, or 2 are valid"))

type num = | NumZero | NumOne | NumTwo

type fill = | FillZero | FillOne | FillTwo

type color = | ColorZero | ColorOne | ColorTwo

type shape = | ShapeZero | ShapeOne | ShapeTwo

let num_to_attr = function
    | NumZero -> AttrZero
    | NumOne -> AttrOne
    | NumTwo -> AttrTwo

let attr_to_num = function
    | AttrZero -> NumZero
    | AttrOne -> NumOne
    | AttrTwo -> NumTwo

let fill_to_attr = function
    | FillZero -> AttrZero
    | FillOne -> AttrOne
    | FillTwo -> AttrTwo

let attr_to_fill = function
    | AttrZero -> FillZero
    | AttrOne -> FillOne
    | AttrTwo -> FillTwo

let color_to_attr = function
    | ColorZero -> AttrZero
    | ColorOne -> AttrOne
    | ColorTwo -> AttrTwo

let attr_to_color = function
    | AttrZero -> ColorZero
    | AttrOne -> ColorOne
    | AttrTwo -> ColorTwo

let shape_to_attr = function
    | ShapeZero -> AttrZero
    | ShapeOne -> AttrOne
    | ShapeTwo -> AttrTwo

let attr_to_shape = function
    | AttrZero -> ShapeZero
    | AttrOne -> ShapeOne
    | AttrTwo -> ShapeTwo

let num_to_int x = num_to_attr x |> attr_to_int
let fill_to_int x = fill_to_attr x |> attr_to_int
let color_to_int x = color_to_attr x |> attr_to_int
let shape_to_int x = shape_to_attr x |> attr_to_int

let int_to_num x = int_to_attr x |> attr_to_num
let int_to_fill x = int_to_attr x |> attr_to_fill
let int_to_color x = int_to_attr x |> attr_to_color
let int_to_shape x = int_to_attr x |> attr_to_shape

type t = {
    num: num;
    fill: fill;
    color: color;
    shape: shape;
}

let num x = x.num
let fill x = x.fill
let color x  = x.color
let shape x = x.shape

let num_attr x = num x |> num_to_attr
let fill_attr x = fill x |> fill_to_attr
let color_attr x = color x |> color_to_attr
let shape_attr x = shape x |> shape_to_attr


let make n f c s = {
    num = int_to_num n;
    fill = int_to_fill f;
    color = int_to_color c;
    shape = int_to_shape s;
}

let of_int n =
    let v = Base_conv.base_list_of_int ~base:3 ~size:4 n |> Array.of_list in
    make v.(0) v.(1) v.(2) v.(3)

let of_int_opt n =
    if n >= 0 && n < 81 then Some (of_int n)
    else None

let of_int_list l = List.map of_int_opt l

let to_int x =
    (num_to_int x.num) * 27 +
    (fill_to_int x.fill) * 9 +
    (color_to_int x.color) * 3 +
    (shape_to_int x.shape)

let to_int_opt = function
    | Some c -> to_int c
    | None -> 81

let to_int_list l = List.map to_int_opt l

let to_string x =
    Printf.sprintf "{ n: %d, f: %d, c: %d, s: %d }"
        (num_to_int x.num)
        (fill_to_int x.fill)
        (color_to_int x.color)
        (shape_to_int x.shape)

let compare x0 x1 = compare (to_int x0) (to_int x1)

let equal x0 x1 =
    List.for_all (fun f -> f x0 == f x1) [num_attr; fill_attr; color_attr; shape_attr]

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
    ) [num_attr; fill_attr; color_attr; shape_attr]

let is_triple_set (c0, c1, c2) = is_set c0 c1 c2

let complete_attr a0 a1 =
    match (a0, a1) with
    | (AttrZero, AttrZero) | (AttrOne, AttrOne) | (AttrTwo, AttrTwo) -> a0
    | (AttrOne, AttrTwo) | (AttrTwo, AttrOne) -> AttrZero
    | (AttrZero, AttrTwo) | (AttrTwo, AttrZero) -> AttrOne
    | (AttrZero, AttrOne) | (AttrOne, AttrZero) -> AttrTwo

let complete c0 c1 = {
    num = attr_to_num @@ complete_attr (num_to_attr c0.num) (num_to_attr c1.num);
    fill = attr_to_fill @@ complete_attr (fill_to_attr c0.fill) (fill_to_attr c1.fill);
    color = attr_to_color @@ complete_attr (color_to_attr c0.color) (color_to_attr c1.color);
    shape = attr_to_shape @@ complete_attr (shape_to_attr c0.shape) (shape_to_attr c1.shape);
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

let exists_set cards =
    let tg = Combinatorics.triple_generator ~comp:compare cards in
    let rec aux = function
        | Some (triple) -> if is_triple_set triple then true
        else aux (tg ())
        | None -> false
    in aux (tg ())
