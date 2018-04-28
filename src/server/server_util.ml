open CCList.Infix

let chars = (48 -- 57) @ (97 -- 122) @ (65 -- 90) >|= Char.chr |> Array.of_list

let form_decode body = Uri.query_of_encoded body

let form_value body key =
  let data = form_decode body in
  match CCList.find_pred (fun (x, _) -> x = key) data with
  | Some (_, tl) -> CCList.head_opt tl
  | None -> None

let string_to_char_list s =
  let rec aux ac i =
    if i = -1 then ac
    else
      aux (s.[i] :: ac) (pred i)
  in aux [] (String.length s - 1)

let chars_of_base b =
    if 1 < b && b <= Array.length chars then
        Array.sub chars 0 b
    else
        raise (Invalid_argument ("Invalid base value: " ^ string_of_int b))

let int_of_base s b =
  let my_chars = chars_of_base b in
  let rec aux l pos ac =
    match l with
    | [] -> ac
    | hd :: tl ->
      match CCArray.find_idx (fun x -> x = hd) my_chars with
      | Some ((idx, x)) -> aux tl (pos+1) (ac + (idx * (CCInt.pow b pos)))
      | None -> raise (Invalid_argument ("Invalid base " ^ string_of_int b ^ " char: " ^ String.make 1 hd))
  in
  let len = String.length s in
  let (i, ps) = if len > 0 && (String.get s 0 = String.get "-" 0) then
    ((-1), String.sub s 1 (len - 1))
  else
    (1, s)
  in
  i * (aux (CCList.rev (string_to_char_list ps)) 0 0)

(* decode from base-36 positive string to base-10 integer *)
let int_of_base36 s = int_of_base s 36

let base_of_int n b =
  let my_chars = chars_of_base b in
  let rec aux n ac =
    if n < 0 then
      raise (Invalid_argument ("Negative values unsupported"))
    else if n = 0 then
      if ac = "" then (String.make 1 (my_chars.(0))) else ac
    else
      let next = n / b in
      let remainder = n mod b in
      let chr = my_chars.(remainder) in
      aux next (String.make 1 chr) ^ ac
  in
  let (prefix, m) = if n < 0 then ("-", (-1)*n) else ("", n) in
  prefix ^ (aux m "")

(* encode from base-10 positive integer to base-36 string *)
let base36_of_int n = base_of_int n 36
