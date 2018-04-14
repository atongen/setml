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

let p i s =
  ignore(print_endline ((string_of_int i) ^ ": " ^ s))

(*
    Beware of js call stack depth here...
    https://stackoverflow.com/questions/7826992/browser-javascript-stack-size-limit
    50 cards is 19600 combinations of 3
*)
let make board_size m =
  let a = Card.deck () in
  let r = a |> knuth_shuffle |> Array.to_list in
  let rec aux acc s m = function
    | [] ->
      p 0 "empty list";
      acc
    | (hd :: tl) as l ->
      p (List.length l) "starting with list";
      let board = firstk s l in
      if List.length board < s then (
        p (List.length board) "board too short";
        acc @ l
      ) else if Card.exists_set board then (
        (* there is a set on the board, move on *)
        p 0 "set exists on board";
        aux (hd :: acc) s m tl
      ) else if m < 0 || List.length l <= m then (
        if Card.exists_set l then (
          (* no set on board, but one exists in remainder of deck, shuffle and retry *)
          p 0 "YES set exists in entire list";
          aux acc s m (shuffle l)
        ) else (
          (* no set exists in rest of list, return current results *)
          p 0 "NO set exists in entire list";
          acc @ l
        )
      ) else (
        p 0 "retry!";
        aux [] s m (a |> knuth_shuffle |> Array.to_list)
      )
  in
  aux [] board_size m r |> Array.of_list
