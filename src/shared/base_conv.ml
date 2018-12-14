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

let int_of_base36_opt s =
    try Some (int_of_base36 s)
    with Invalid_argument _e -> None

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

(* encode base-10 positive integer into list of length size of base ints *)
let base_list_of_int ~base ~size n =
    let max = pow base size in
    let rec aux n ac =
        if n < 0 then
            raise (Invalid_argument "n is less than zero")
        else if n > max then
            raise (Invalid_argument (Printf.sprintf "n (%d) > max (%d = %d^%d)" n max base size))
        else if n = 0 then ac else
            let next = n / base in
            let remainder = n mod base in
            aux next (remainder :: ac)
    in
    let rec fill l n v =
        if List.length l < n then
            fill (v :: l) n v
        else l
    in
    fill (aux n []) size 0

let int_of_base_list ~base l =
    let rec aux l pos ac =
        match l with
        | [] -> ac
        | hd :: tl ->
            if hd < 0 || hd >= base then
                raise (Invalid_argument ("Invalid value for base: " ^ string_of_int hd))
            else
                aux tl (pos+1) (ac + (hd * (pow base pos)))
    in
    aux (List.rev l) 0 0
