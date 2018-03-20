let int_of_base n b =
    b

(* https://codegolf.stackexchange.com/questions/8606/print-integers-in-any-base-up-to-36 *)
let rec base_of_int n b =
    let s = match n with
    | 0 -> "0"
    | n -> (if 0 = n / b then ""
            else base_of_int n b)
    in
    s ^ String.make 1 (Char.chr (n mod b + if n mod b>9 then 55 else 48))