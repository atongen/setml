open Messages

let board_cards_compact (board_cards: board_card_data list) =
    let rec aux acc = function
    | [] -> acc
    | (hd: board_card_data) :: tl -> match hd.card with
        | Some x ->
            let c = { idx = hd.idx; card = x } in
            aux (c :: acc) tl
        | None -> aux acc tl
    in
    aux [] (List.rev board_cards)

let board_cards_exists_set (board_cards: board_card_data list) =
    let cards = board_cards_compact board_cards in
    let c = List.map (fun (cd: card_data) -> cd.card) cards in
    Card.exists_set c

let board_cards_is_set ((bc0: board_card_data), (bc1: board_card_data), (bc2: board_card_data)): (card_data * card_data * card_data) option =
    let m idx card = { idx; card } in
    match (bc0.card, bc1.card, bc2.card) with
    | (Some c0, Some c1, Some c2) ->
        if Card.is_set c0 c1 c2 then
            Some (m bc0.idx c0, m bc1.idx c1, m bc2.idx c2)
        else None
    | (_, _, _) -> None

let board_cards_list_is_set l =
    if List.length l == 3 then
        let a = Array.of_list l in
        let c = (a.(0), a.(1), a.(2)) in
        board_cards_is_set c
    else None

let board_cards_next_set (board_cards: board_card_data list) =
    let cards = board_cards_compact board_cards in
    let cc = (fun (cd0: card_data) (cd1: card_data) -> Card.compare cd0.card cd1.card) in
    let tg = Combinatorics.triple_generator ~comp:cc cards in
    let rec aux = function
    | Some (cd0, cd1, cd2) ->
        if Card.is_set cd0.card cd1.card cd2.card then
            Some(cd0, cd1, cd2)
        else
            aux (tg ())
    | None -> None
    in aux (tg ())
