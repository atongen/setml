open CCList.Infix

let chars = (48 -- 57) @ (97 -- 122) >|= Char.chr |> CCArray.of_list

let form_decode body = Uri.query_of_encoded body

let form_value body key =
  let data = form_decode body in
  match CCList.find_pred (fun (x, _) -> x = key) data with
  | Some (_, tl) -> CCList.head_opt tl
  | None -> None

(* decode from base-36 positive string to base-10 integer *)
let string_to_char_list s =
  let rec aux ac i =
    if i = -1 then ac
    else
      aux (s.[i] :: ac) (pred i)
  in aux [] (String.length s - 1)

let int_of_base36 b =
  let rec aux b pos ac =
    match b with
    | [] -> ac
    | hd :: tl ->
      match CCArray.find_idx (fun x -> x = hd) chars with
      | Some ((idx, x)) -> aux tl (pos+1) (ac + (idx * (CCInt.pow 36 pos)))
      | None -> raise (Invalid_argument ("Invalid base36 char: " ^ String.make 1 hd))
  in
  aux (CCList.rev (string_to_char_list b)) 0 0

(* encode from base-10 positive integer to base-36 string *)
let base36_of_int n =
  let rec aux n ac =
    if n < 0 then
      raise (Invalid_argument ("Negative values unsupported"))
    else if n = 0 then
      if ac = "" then "0" else ac
    else
      let next = n / 36 in
      let remainder = n mod 36 in
      let chr = chars.(remainder) in
      aux next (String.make 1 chr) ^ ac
  in
  aux n ""
