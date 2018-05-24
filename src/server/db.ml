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

  let make_insert_cards_query_str ?(offset=0) table_name game_id card_ids =
    let rows = List.mapi (fun idx card_id ->
        Printf.sprintf "(%d,%d,%d)" game_id (idx + offset) card_id
      ) card_ids in
    let values = String.concat "," rows in
    Printf.sprintf "insert into %s (game_id, idx, card_id) values %s;" table_name values

  let create_game_cards_query game_id =
    let open CCList.Infix in
    let card_ids = Shared_util.shuffle_list (0 --^ 81) in
    let query = make_insert_cards_query_str "game_cards" game_id card_ids in
    Caqti_request.exec ~oneshot:true Caqti_type.unit query

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

  let make_update_cards_query_str table =
      table |> (Printf.sprintf
      {eos|
        WITH rows AS (
            update %s t
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
        SELECT count(*) FROM rows;
      |eos})

  let update_board_cards_query =
    let args10 = Caqti_type.(let (&) = tup2 in int & int & int & int & int & int & int & int & int & int) in
    Caqti_request.find args10 Caqti_type.int (make_update_cards_query_str "board_cards")

  let update_game_cards_query =
    let args10 = Caqti_type.(let (&) = tup2 in int & int & int & int & int & int & int & int & int & int) in
    Caqti_request.find args10 Caqti_type.int (make_update_cards_query_str "game_cards")

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
end

let query_int (module C : Caqti_lwt.CONNECTION) q =
  C.find (Q.generic_int_query q) ()

let query_bool (module C : Caqti_lwt.CONNECTION) q =
  C.find (Q.generic_bool_query q) ()

let create_game (module C : Caqti_lwt.CONNECTION) () =
  C.start () >>=? fun () ->
  C.find Q.create_game_query () >>=? fun game_id ->
  C.exec (Q.create_game_cards_query game_id) () >>=? fun () ->
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
  C.find Q.update_board_cards_query (idx0, (card0_id, (new_card_0_id, (idx1, (card1_id, (new_card_1_id, (idx2, (card2_id, (new_card_2_id, game_id))))))))) >>=? fun num_updated ->
  if num_updated != 3 then C.rollback () else C.commit ()

let shuffle_board (module C : Caqti_lwt.CONNECTION) (game_id, player_id) =
  C.start () >>=? fun () ->
  C.exec (Q.set_transaction_mode_query "serializable") () >>=? fun () ->
  C.find Q.find_game_card_idx_query game_id >>=? fun card_idx ->

  (* ensure we have at least board size 12 + 3 cards left to swap *)
  if card_idx < 66 then C.rollback () else

  (* get a list of all (idx, card_id) for this game *)
  C.collect_list Q.find_game_cards_query (game_id, card_idx, 81 - card_idx) >>=? fun cards_list ->

  (* get a list of all (idx, card_id) for cards currently on the board *)
  C.collect_list Q.find_random_board_cards_query (game_id, 3) >>=? fun random_board_cards ->
  if List.length random_board_cards < 3 then C.rollback () else

  (*
    # GAME CARDS
    - query all game_cards card_ids for game
    - delete from game_cards where card_id is the board cards we want to shuffle out
    - delete from game_cards where idx is > game card_idx
    - pop 3 from game_cards "deck" and insert where we want to replace from board
    - append replaced cards from board at end of "deck"
    - insert new "deck" rows into game cards
    # BOARD CARDS
    - similar to "move", replace 3 (idx, card_id) tuples in board_card rows
      with replaced game cards from above
    # SHUFFLES
    - insert row into new table "shuffles" recording the game, player and
      the number of sets on the board when it occured
  *)


  C.commit ()

let is_game_over (module C : Caqti_lwt.CONNECTION) game_id =
  (*
  if there is a set present on board_cards, then false
  if there is a set present on remaining game_cards, then false
  else true
  *)
  Lwt.return_unit

let update_player_name (module C : Caqti_lwt.CONNECTION) (player_id, name) =
  C.exec Q.update_player_name_query (name, player_id)

let find_board_cards (module C : Caqti_lwt.CONNECTION) game_id =
  C.collect_list Q.find_board_cards_query game_id

let find_scoreboard (module C : Caqti_lwt.CONNECTION) game_id =
  C.collect_list Q.find_scoreboard_query game_id

let delete_all (module C : Caqti_lwt.CONNECTION) () =
  C.start () >>=? fun () ->
  C.exec (Q.generic_exec_query "delete from games;") () >>=? fun () ->
  C.exec (Q.generic_exec_query "delete from players;") () >>=? fun () ->
  C.commit ()
