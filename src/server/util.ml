open CCList.Infix

let chars = (48 -- 57) @ (97 -- 122) >|= Char.chr |> CCArray.of_list

(* from base-36 positive string to base-10 integer *)
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
            | None -> raise (Invalid_argument ("s"))
    in
    aux (CCList.rev (string_to_char_list b)) 0

(* from base-10 positive integer to base-36 string *)
let rec base36_string_of_int n =
    let aux n ac =
        if n < 0 then
            ac
        else if n = 0 then
            "0" ^ ac
        else
            let code = n mod 36 in
            let chr = chars.(code) in
            (String.make 1 chr) ^ ac
    in
    aux n ""