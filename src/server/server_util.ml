let form_decode body = Uri.query_of_encoded body

let form_value body key =
    let data = form_decode body in
    match CCList.find_pred (fun (x, _) -> x = key) data with
    | Some (_, tl) -> CCList.head_opt tl
    | None -> None

let get_or a i d =
    match CCArray.get_safe a i with
    | Some v -> v
    | None -> d
