let round v =
    if v < 0.0 then
        ceil (v -. 0.5)
    else
        floor (v +. 0.5)

let roundi v = int_of_float @@ round v

let shuffle_array a =
  let n = Array.length a in
  let a = Array.copy a in
  for i = n - 1 downto 1 do
    let k = Random.int (i+1) in
    let x = a.(k) in
    a.(k) <- a.(i);
    a.(i) <- x
  done;
  a

let rec shuffle_list = function
  | [] -> []
  | [x] -> [x]
  | l ->
    let (before, after) = List.partition (fun elt -> Random.bool ()) l in
    List.rev_append (shuffle_list before) (shuffle_list after)

let rec firstk k = function
  | [] -> []
  | hd :: tl -> if k=1 then [hd] else
      hd :: firstk (k-1) tl
