open Shared_util

let choose n k =
  let rec aux nf kf acc =
    if kf = 0. || nf = kf then (
      acc
    ) else
      aux (nf -. 1.) (kf -. 1.) (acc *. (nf /. kf))
  in
  if n < k || k < 0 then
    0
  else if n - k < k then
    roundi (aux (float_of_int n) (float_of_int (n-k)) 1.)
  else
    roundi (aux (float_of_int n) (float_of_int k) 1.)

(*
 * Index of Combinations in Lexicographical Order (McCaffrey)
 * https://stackoverflow.com/questions/127704/algorithm-to-return-all-combinations-of-k-elements-from-n#answer-127856
 *)
let combination_maccaffery l k x =
  let rec maximize a b x =
    if (choose a b) <= x then a else maximize (a-1) b x
  in
  let rec iterate n x i = match i with
    | 0 -> []
    | i ->
      let max = maximize n i x in
      max :: iterate n (x - (choose max i)) (i-1)
  in
  if x < 0 then
    raise (Invalid_argument "Index must not be less than zero")
  else (
    let idxs = iterate (List.length l) x k in
    let ls = List.sort (-) idxs in
    let nth = List.nth l in
    List.map nth ls)

let comb_generator l k =
  let s = choose (List.length l) k in
  let i = ref 0 in
  let gen () =
    if !i < s then (
      let result = Some(combination_maccaffery l k !i) in
      i := !i+1;
      result
    ) else None
  in gen

let comb0 l k =
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

let rec comb1 l k =
  if k <= 0 then [ [] ]
  else match l with
    | [] -> []
    | hd :: tl ->
      let with_h = List.map (fun l -> hd :: l) (comb1 tl (k-1)) in
      let without_h = comb1 tl k in
      with_h @ without_h

let triples ?(comp=compare) items =
  let s = List.sort_uniq comp items in
  let arrays = List.map Array.of_list (comb0 s 3) in
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> if Array.length hd = 3 then
        aux ((hd.(0), hd.(1), hd.(2)) :: acc) tl
      else
        raise (Invalid_argument("Triple combination with length other than 3"))
  in aux [] arrays


let triple_generator ?(comp=compare) items =
  let s = List.sort_uniq comp items in
  let gen = comb_generator s 3 in
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
