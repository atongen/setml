let form_decode body = Uri.query_of_encoded body

let form_value body key =
    let data = form_decode body in
    match CCList.find_pred (fun (x, _) -> x = key) data with
    | Some (_, tl) -> CCList.head_opt tl
    | None -> None

let form_value_int body key =
    match form_value body key with
    | Some s -> int_of_string_opt s
    | None -> None

let form_value_int_of_base36 body key =
    match form_value body key with
    | Some s -> Shared.Base_conv.int_of_base36_opt s
    | None -> None

let get_or a i d =
    match CCArray.get_safe a i with
    | Some v -> v
    | None -> d
