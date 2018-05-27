open Lwt.Infix
open Shared

let (>>=?) m f =
  m >>= function
  | Ok x -> f x
  | Error err -> Lwt.return (Error err)

let board_size = 12

module Q = struct
  let set_transaction_mode_query mode =
    Caqti_request.exec ~oneshot:true Caqti_type.unit (Printf.sprintf "set transaction isolation level %s;" mode)

  let generic_exec_query q = Caqti_request.exec ~oneshot:true Caqti_type.unit q

  let generic_int_query q = Caqti_request.find ~oneshot:true Caqti_type.unit Caqti_type.int q

  let generic_bool_query q = Caqti_request.find ~oneshot:true Caqti_type.unit Caqti_type.bool q

  let make_insert_cards_query_str table_name game_id card_list =
    let rows = List.map (fun (idx, card_id) ->
        Printf.sprintf "(%d,%d,%d)" game_id idx card_id
      ) card_list in
    let values = String.concat "," rows in
    Printf.sprintf
    {eos|
        with rows as (
            insert into %s (game_id, idx, card_id)
            values %s
            returning id
        ) select count(*) from rows;
    |eos}
    table_name values

  let create_game_cards_query game_id =
    let open CCList.Infix in
    let card_ids = Shared_util.shuffle_list (0 --^ 81) in
    let card_list = List.mapi (fun idx card_id -> (idx, card_id)) card_ids in
    let query = make_insert_cards_query_str "game_cards" game_id card_list in
    Caqti_request.find ~oneshot:true Caqti_type.unit Caqti_type.int query

  let update_game_cards_query game_id card_list =
    let query = make_insert_cards_query_str "game_cards" game_id card_list in
    Caqti_request.find ~oneshot:true Caqti_type.unit Caqti_type.int query

  let create_game_query =
    Caqti_request.find Caqti_type.unit Caqti_type.int
      "insert into games default values returning id"

  let create_board_cards_query =
    Caqti_request.exec Caqti_type.(tup2 int int)
      {eos|
        insert into board_cards (game_id, idx, card_id)
        select game_id, idx, card_id
        from game_cards
        where game_id = ?
        order by idx asc
        limit ?
      |eos}

  let find_game_card_idx_query =
    Caqti_request.find Caqti_type.int Caqti_type.int
      {eos|
        select card_idx
        from games
        where id = ?
      |eos}

  let increment_game_card_idx_query =
    Caqti_request.find Caqti_type.(tup2 int int) Caqti_type.int
      {eos|
        update games
        set card_idx = card_idx + ?
        where id = ?
        returning card_idx;
      |eos}

  let game_exists_query =
    Caqti_request.find Caqti_type.int Caqti_type.bool
      "select exists (select 1 from games where id = ?)"

  let create_player_query =
    Caqti_request.find Caqti_type.unit Caqti_type.int
      "insert into players default values returning id"

  let player_exists_query =
    Caqti_request.find Caqti_type.int Caqti_type.bool
      "select exists (select 1 from players where id = ?)"

  let game_player_presence_query =
    Caqti_request.exec Caqti_type.(tup3 int int bool)
      {eos|
        insert into games_players (game_id, player_id, presence, updated_at)
        values (?, ?, ?, now())
        on conflict (game_id, player_id)
        do update set presence = excluded.presence,
        updated_at = excluded.updated_at;
      |eos}

  let create_move_query =
    let args8 = Caqti_type.(let (&) = tup2 in int & int & int & int & int & int & int & int) in
    Caqti_request.exec args8
      {eos|
        insert into moves (
            game_id,
            player_id,
            idx0,
            card0_id,
            idx1,
            card1_id,
            idx2,
            card2_id
        ) values (
            ?, ?, ?, ?, ?, ?, ?, ?
        );
      |eos}

  let update_board_cards_query =
    (*let args10 = Caqti_type.(let (&) = tup2 in int & int & int & int & int & int & int & int & int & int) in*)
    let args = Caqti_type.(tup4 (tup3 int int int) (tup3 int int int) (tup3 int int int) int) in
    Caqti_request.find args Caqti_type.int
    {eos|
        with rows as (
            update board_cards t
            set card_id = m.new_card_id::int
            from (values
                (?, ?, ?),
                (?, ?, ?),
                (?, ?, ?)
            ) as m (
                idx,
                old_card_id,
                new_card_id
            )
            where t.game_id = ?
                and t.idx = m.idx::int
                and t.card_id = m.old_card_id::int
            returning 1
        )
        select count(*) from rows;
      |eos}

  let find_game_cards_query =
    Caqti_request.collect Caqti_type.(tup3 int int int) Caqti_type.(tup2 int int)
      {eos|
        select idx, card_id
        from game_cards
        where game_id = ?
        and idx >= ?
        order by idx asc
        limit ?
      |eos}

    let update_player_name_query =
        Caqti_request.exec Caqti_type.(tup2 string int)
        {eos|
            update players
            set name = ?
            where id = ?
            limit 1;
        |eos}

    let find_board_cards_query =
        Caqti_request.collect Caqti_type.int Caqti_type.int
        {eos|
            select card_id
            from board_cards
            where game_id = ?
            order by idx asc;
        |eos}

    let find_scoreboard_query =
        Caqti_request.collect Caqti_type.int Caqti_type.(tup4 int string bool int)
        {eos|
            select
                gp.player_id,
                p.name,
                gp.presence,
                (
                    select count(*)
                    from moves
                    where moves.game_id = gp.game_id
                    and moves.player_id = gp.player_id
                ) as score
            from games_players gp
            inner join players p
            on gp.player_id = p.id
            where gp.game_id = ?;
        |eos}

    let find_random_board_cards_query =
        Caqti_request.collect Caqti_type.(tup2 int int) Caqti_type.(tup2 int int)
        {eos|
            select idx, card_id
            from board_cards
            where game_id = ?
            and card_id < 81
            order by random()
            limit ?;
        |eos}

    let delete_game_cards_for_shuffle =
        let args = Caqti_type.(let (&) = tup2 in (int & (int & int) & (int & int) & (int & int) & int)) in
        Caqti_request.find args Caqti_type.int
        {eos|
            with rows as (
                delete from game_cards
                where game_id = ?
                and (
                    (idx = ? and card_id = ?)
                    or (idx = ? and card_id = ?)
                    or (idx = ? and card_id = ?)
                    or (idx >= ?)
                )
                returning id
            ) select count(*) from rows;
        |eos}

  let create_shuffle_query =
    Caqti_request.exec Caqti_type.(tup3 int int int)
      {eos|
        insert into shuffles (
            game_id,
            player_id,
            sets_on_board
        ) values (
            ?, ?, ?
        );
      |eos}
end

let query_int (module C : Caqti_lwt.CONNECTION) q =
  C.find (Q.generic_int_query q) ()

let query_bool (module C : Caqti_lwt.CONNECTION) q =
  C.find (Q.generic_bool_query q) ()

let create_game (module C : Caqti_lwt.CONNECTION) () =
  C.start () >>=? fun () ->
  C.find Q.create_game_query () >>=? fun game_id ->
  C.find (Q.create_game_cards_query game_id) () >>=? fun _ ->
  C.exec Q.create_board_cards_query (game_id, board_size) >>=? fun () ->
  C.find Q.increment_game_card_idx_query (board_size, game_id) >>=? fun _ ->
  C.commit () >>=? fun () ->
  Lwt.return_ok game_id

let game_exists (module C : Caqti_lwt.CONNECTION) game_id =
  C.find Q.game_exists_query game_id

let create_player (module C : Caqti_lwt.CONNECTION) () =
  C.find Q.create_player_query ()

let player_exists (module C : Caqti_lwt.CONNECTION) player_id =
  C.find Q.player_exists_query player_id

let game_player_presence (module C : Caqti_lwt.CONNECTION) args =
  C.exec Q.game_player_presence_query args

let increment_game_card_idx (module C : Caqti_lwt.CONNECTION) (game_id, offset) =
  C.find Q.increment_game_card_idx_query (offset, game_id) >>=? fun card_idx ->
  Lwt.return_ok card_idx

let create_move (module C : Caqti_lwt.CONNECTION) (game_id, player_id, idx0, card0, idx1, card1, idx2, card2) =
  let card0_id, card1_id, card2_id = (Card.to_int card0, Card.to_int card1, Card.to_int card2) in
  C.start () >>=? fun () ->
  C.exec (Q.set_transaction_mode_query "serializable") () >>=? fun () ->
  C.exec Q.create_move_query (game_id, (player_id, (idx0, (card0_id, (idx1, (card1_id, (idx2, card2_id))))))) >>=? fun () ->
  C.find Q.increment_game_card_idx_query (3, game_id) >>=? fun card_idx ->
  C.collect_list Q.find_game_cards_query (game_id, card_idx, 3) >>=? fun cards_list ->
  let card_ids = List.map (fun (_, card_id) -> card_id) cards_list |> Array.of_list in
  let new_card_0_id = Server_util.get_or card_ids 0 81 in
  let new_card_1_id = Server_util.get_or card_ids 1 81 in
  let new_card_2_id = Server_util.get_or card_ids 2 81 in
  C.find Q.update_board_cards_query ((idx0, card0_id, new_card_0_id), (idx1, card1_id, new_card_1_id), (idx2, card2_id, new_card_2_id), game_id) >>=? fun num_updated ->
  if num_updated != 3 then
    C.rollback () >>=? fun () ->
    ignore(print_endline "card_idx less than 66");
    Lwt.return_ok false
  else
    C.commit () >>=? fun () ->
    Lwt.return_ok true

let shuffle_board (module C : Caqti_lwt.CONNECTION) (game_id, player_id) =
  C.start () >>=? fun () ->
  C.exec (Q.set_transaction_mode_query "serializable") () >>=? fun () ->
  C.find Q.find_game_card_idx_query game_id >>=? fun deck_card_idx ->
  (* include board in deck *)
  let card_idx = deck_card_idx - 12 in

  (* ensure we have at least board size 12 + 3 cards left to swap *)
  if card_idx < 0 || card_idx >= 66 then
    C.rollback () >>=? fun () ->
    ignore(print_endline "card_idx less than 66");
    Lwt.return_ok false
  else

  (* get a list of all (idx, card_id) remaining for this game (including the board) *)
  C.collect_list Q.find_game_cards_query (game_id, card_idx, 81 - card_idx) >>=? fun cards_list ->

  (* get a list of all (idx, card_id) for cards currently on the board *)
  C.collect_list Q.find_random_board_cards_query (game_id, 3) >>=? fun random_board_cards ->
  if List.length random_board_cards < 3 then
    C.rollback () >>=? fun () ->
    ignore(print_endline "random board card length less than 3");
    Lwt.return_ok false
  else

  (* delete the 3 random board cards from game_cards, and also cards idx higher than card_idx *)
  let to_delete_list =
    let rec aux acc = function
    | [] -> acc
    | (_, bc_card_id) :: tl ->
      let ol = List.find_opt (fun (_, g_card_id) ->
        bc_card_id = g_card_id
      ) cards_list in
      match ol with
      | Some x -> aux (x :: acc) tl
      | None -> aux acc tl
    in
    aux [] (List.rev random_board_cards)
  in

  ignore(print_endline(Shared_util.card_list_to_string "random_board_cards" random_board_cards));
  ignore(print_endline(Shared_util.card_list_to_string "cards_list" cards_list));
  ignore(print_endline(Shared_util.card_list_to_string "to_delete_list" to_delete_list));

  if List.length to_delete_list < 3 then
    C.rollback () >>=? fun () ->
    ignore(print_endline "board cards to delete length less than 3");
    Lwt.return_ok false
  else

  let to_delete = Array.of_list to_delete_list in

  C.find Q.delete_game_cards_for_shuffle (game_id, (to_delete.(0), (to_delete.(1), (to_delete.(2), deck_card_idx)))) >>=? fun num_deleted ->

  let num_should_delete = 3 + 81 - deck_card_idx in
  if num_deleted < num_should_delete then
    C.rollback () >>=? fun () ->
    ignore(print_endline @@ Printf.sprintf "should delete %d, but actually deleted %d" num_should_delete num_deleted);
    Lwt.return_ok false
  else

  let deck = CCList.drop 12 cards_list in

  (* add 3 from card_idx+12 to card_idx+14 top of deck into where deleted board cards were *)
  let deleted_idx = List.map (fun (idx, _) -> idx) to_delete_list in
  let new_card_ids = List.map (fun (_, card_id) -> card_id) (CCList.take 3 deck) in
  let new_board_cards = CCList.combine deleted_idx new_card_ids in

  (* shift card_idx+12+3 to 80-3 (78, 79, 80) forward by 3 *)
  let shifted_deck =
    let rec aux acc = function
      | [] -> acc
      | (idx, card_id) :: tl ->
        if idx < deck_card_idx+3 then
          aux acc tl
        else
          aux ((idx-3, card_id) :: acc) tl
    in
    aux [] (List.rev cards_list)
  in

  (* add 3 deleted board cards to idx 78, 79, 80 *)
  let end_of_deck = List.mapi (fun i (_, card_id) -> (78+i, card_id)) to_delete_list in

  let to_add = new_board_cards @ shifted_deck @ end_of_deck in
  C.find (Q.update_game_cards_query game_id to_add) () >>=? fun num_added ->

  ignore(print_endline @@ Printf.sprintf "length of to_add: %d" (List.length to_add));

  let num_should_added = 3 + 81 - deck_card_idx in
  ignore(print_endline @@ Printf.sprintf "num_should_added: %d" num_should_added);
  if num_added < num_should_added then
    C.rollback () >>=? fun () ->
    ignore(print_endline @@ Printf.sprintf "added %d, but should have added: %d" num_added num_should_added);
    Lwt.return_ok false
  else

  let board_update = List.map2 (fun (idx, old_card_id) (_, new_card_id) ->
    (idx, old_card_id, new_card_id)
  ) random_board_cards new_board_cards |> Array.of_list in

  C.find Q.update_board_cards_query (board_update.(0), board_update.(1), board_update.(2), game_id) >>=? fun num_updated ->

  if num_updated < 3 then
    C.rollback () >>=? fun () ->
    ignore(print_endline "unable to update board cards after shuffling game deck");
    Lwt.return_ok false
  else

  let sets_on_board = List.map (fun (_, card_id) -> card_id) (CCList.take 12 cards_list)
    |> Card.of_int_list
    |> Shared_util.compact
    |> Card.count_sets
  in

  C.exec Q.create_shuffle_query (game_id, player_id, sets_on_board) >>=? fun () ->
  C.commit () >>=? fun () ->
  Lwt.return_ok true

let is_game_over (module C : Caqti_lwt.CONNECTION) game_id =
  C.find Q.find_game_card_idx_query game_id >>=? fun card_idx ->
  if card_idx >= 81 then
    Lwt.return_ok true
  else
    C.collect_list Q.find_game_cards_query (game_id, card_idx, 81 - card_idx) >>=? fun cards_list ->
    Lwt.return_ok (not (List.map (fun (_, card_id) -> card_id) cards_list
        |> Card.of_int_list
        |> Shared_util.compact
        |> Card.exists_set))

let update_player_name (module C : Caqti_lwt.CONNECTION) (player_id, name) =
  C.exec Q.update_player_name_query (name, player_id)

let find_board_cards (module C : Caqti_lwt.CONNECTION) game_id =
  C.collect_list Q.find_board_cards_query game_id

let find_game_cards (module C : Caqti_lwt.CONNECTION) ?(offset=0) game_id =
    C.collect_list Q.find_game_cards_query (game_id, offset, 81 - offset)

let find_scoreboard (module C : Caqti_lwt.CONNECTION) game_id =
  C.collect_list Q.find_scoreboard_query game_id

let delete_all (module C : Caqti_lwt.CONNECTION) () =
  C.start () >>=? fun () ->
  C.exec (Q.generic_exec_query "delete from games;") () >>=? fun () ->
  C.exec (Q.generic_exec_query "delete from players;") () >>=? fun () ->
  C.commit ()
