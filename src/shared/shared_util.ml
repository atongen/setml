let round v =
    if v < 0.0 then
        ceil (v -. 0.5)
    else
        floor (v +. 0.5)

let roundi v = int_of_float @@ round v

let roundis v = string_of_int @@ roundi v

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

let random_sample ar n =
    let a = [||] in
    while Array.length a < n do
        let i = Random.int n in
        if not (Array.mem i a) then
        Array.set a (Array.length a) i
    done;
    Array.map (fun i -> ar.(i)) a

let random_sample_list l n =
    random_sample (Array.of_list l) n
    |> Array.to_list

let compact l =
    let rec aux acc = function
    | [] -> acc
    | hd :: tl -> match hd with
        | Some x -> aux (x :: acc) tl
        | None -> aux acc tl
    in
    aux [] (List.rev l)

let card_list_to_string m l =
    let s = List.map (fun (idx, card_id) ->
        Printf.sprintf "(idx: %d, card_id: %d)" idx card_id
    ) l in
    Printf.sprintf "%s: %s" m (String.concat ", " s)
