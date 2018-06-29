let board_cards_compact (board_cards: Messages.board_card_data list) =
    let open Messages in
    let rec aux acc = function
    | [] -> acc
    | (hd: board_card_data) :: tl -> match hd.card with
        | Some x ->
            let c = { idx = hd.idx; card = x } in
            aux (c :: acc) tl
        | None -> aux acc tl
    in
    aux [] (List.rev board_cards)

let board_cards_exists_set (board_cards: Messages.board_card_data list) =
    let cards = board_cards_compact board_cards in
    let open Messages in
    let c = List.map (fun (cd: card_data) -> cd.card) cards in
    Card.exists_set c

let board_cards_are_set (bc0: Messages.board_card_data) (bc1: Messages.board_card_data) (bc2: Messages.board_card_data) =
    let open Messages in
    match (bc0.card, bc1.card, bc2.card) with
    | (Some(c0), Some(c1), Some(c2)) -> Card.is_set c0 c1 c2
    | _ -> false

let c_to_bc (c: Messages.card_data): Messages.board_card_data =
    let open Messages in
    {
        idx = c.idx;
        card = Some c.card;
    }

let board_cards_next_set (board_cards: Messages.board_card_data list) =
    let cards = board_cards_compact board_cards in
    let open Messages in
    let compare_cards = (fun (cd0: card_data) (cd1: card_data) ->
        Card.compare cd0.card cd1.card) in
    let tg = Combinatorics.triple_generator ~comp:compare_cards cards in
    let rec aux = function
    | Some (cd0, cd1, cd2) ->
        if Card.is_set cd0.card cd1.card cd2.card then
            Some(c_to_bc cd0, c_to_bc cd1, c_to_bc cd2)
        else
            aux (tg ())
    | None -> None
    in aux (tg ())
