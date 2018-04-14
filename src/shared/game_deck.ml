type t = Card.t array

let knuth_shuffle a =
  let n = Array.length a in
  let a = Array.copy a in
  for i = n - 1 downto 1 do
    let k = Random.int (i+1) in
    let x = a.(k) in
    a.(k) <- a.(i);
    a.(i) <- x
  done;
  a

let rec shuffle = function
  | [] -> []
  | [x] -> [x]
  | l ->
    let (before, after) = List.partition (fun elt -> Random.bool ()) l in
    List.rev_append (shuffle before) (shuffle after)

let rec firstk k = function
  | [] -> []
  | hd :: tl -> if k=1 then [hd] else
      hd :: firstk (k-1) tl

let make board_size =
  let rec aux acc s = function
    | [] -> acc
    | (hd :: tl) as l ->
      let board = firstk board_size l in
      if List.length board = 0 then
        acc
      else if Card.exists_set board then
        (* there is a set on the board, move on *)
        aux (hd :: acc) s tl
      else if Card.exists_set l then
        (* no set on board, but one exists in remainder of deck, shuffle and retry *)
        aux acc s (shuffle l)
      else
        (* no set exists in rest of list, return current results *)
        acc @ l
  in
  let a = Card.deck () |> knuth_shuffle |> Array.to_list in
  aux [] board_size a |> Array.of_list
