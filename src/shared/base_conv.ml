(* compatibility between ocaml & reasonml/bucklescript (ocaml 4.02.3) *)
type ('a, 'b) result = Ok of 'a | Error of 'b

type sign = Positive | Negative

type t = {
    base: int;
    sign: sign;
    chars: char list
}

let make ~base ?(positive=true) chars = {
    base;
    sign = if positive then Positive else Negative;
    chars;
}

let sign_to_int = function
    | Positive -> 1
    | Negative -> (-1)

let int_to_sign n = if n >= 0 then Positive else Negative

let (--) i j =
    let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j []

let default_char_map = List.map Char.chr ((48 -- 57) @ (97 -- 122) @ (65 -- 90)) |> Array.of_list

let rec find_aux f a i =
    if i = Array.length a then None
    else match f i a.(i) with
        | Some _ as res -> res
        | None -> find_aux f a (i+1)

let find_idx p a = find_aux (fun i x -> if p x then Some (i,x) else None) a 0

let pow a b =
    let rec aux acc = function
        | 1 -> acc
        | n ->
            if n mod 2 = 0
            then aux (acc*acc) (n/2)
            else acc * (aux (acc*acc) (n/2))
    in
    match b with
        | 0 -> if a = 0 then raise (Invalid_argument "pow: undefined value 0^0") else 1
        | b when b < 0 -> raise (Invalid_argument "pow: can't raise int to negative power")
        | b -> aux a b

let string_to_char_list s =
  let rec aux ac i =
    if i = -1 then ac
    else
      aux (s.[i] :: ac) (pred i)
  in aux [] (String.length s - 1)

let chars_of_base b =
    if 1 < b && b <= Array.length default_char_map then
        Array.sub default_char_map 0 b
    else
        raise (Invalid_argument ("Invalid base value: " ^ string_of_int b))

let int_of_base s b =
    let my_chars = chars_of_base b in
    let rec aux l pos ac =
        match l with
        | [] -> ac
        | hd :: tl ->
            match find_idx (fun x -> x = hd) my_chars with
            | Some ((idx, _)) -> aux tl (pos+1) (ac + (idx * (pow b pos)))
            | None -> raise (Invalid_argument ("Invalid base " ^ string_of_int b ^ " char: " ^ String.make 1 hd))
    in
    let len = String.length s in
    let (i, ps) = if len > 0 && (String.get s 0 = String.get "-" 0) then
        ((-1), String.sub s 1 (len - 1))
    else
        (1, s)
    in
    i * (aux (List.rev (string_to_char_list ps)) 0 0)

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

(*
 * new interface functions start below this comment
 *)

let to_int ~base ?(mapping=default_char_map) bc =
    if base < 2 || base > 10 then Error "base must be 2 through 10"  else
    if bc.base > Array.length(mapping) then Error "mapping length must equal base_conv length" else
    let rec aux l pos ac =
        match l with
        | [] -> ac
        | hd :: tl ->
            match find_idx (fun x -> x = hd) mapping with
            | Some ((idx, _)) -> aux tl (pos+1) (ac + (idx * (pow base pos)))
            | None -> raise (Invalid_argument ("Invalid base " ^ string_of_int base ^ " char: " ^ String.make 1 hd))
    in
    let i = sign_to_int bc.sign in
    try
        let result = i * (aux (List.rev bc.chars) 0 0) in
        Ok result
    with Invalid_argument s  -> Error s

let of_int ~base ?(mapping=default_char_map) i =
    let rec aux n ac =
        if n < 0 then
            raise (Invalid_argument ("Negative values unsupported"))
        else if n = 0 then
            if ac = [] then [mapping.(0)] else ac
        else
            let next = n / base in
            let remainder = n mod base in
            let chr = mapping.(remainder) in
            aux next (chr :: ac)
    in
    let sign = int_to_sign i in
    let m = abs i in
    {
        sign;
        chars = aux m [];
        base;
    }
