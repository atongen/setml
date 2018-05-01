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

  let create_game_cards_query game_id =
    let cards = Card.make_board board_size in
    let ids = List.map Card.to_int cards in
    let rows = List.mapi (fun idx card_id ->
        Printf.sprintf "(%d,%d,%d)" game_id idx card_id
      ) ids in
    let query_values = String.concat ", " rows in
    let query = "insert into game_cards (game_id, idx, card_id) values " ^ query_values ^ ";" in
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

  let update_board_card_query =
    let args10 = Caqti_type.(let (&) = tup2 in int & int & int & int & int & int & int & int & int & int) in
    Caqti_request.find args10 Caqti_type.int
      {eos|
        WITH rows AS (
            update board_cards bc
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
            where bc.game_id = ?
                and bc.idx = m.idx::int
                and bc.card_id = m.old_card_id::int
            returning 1
        )
        SELECT count(*) FROM rows;
      |eos}

  let find_game_cards_query =
    Caqti_request.collect Caqti_type.(tup3 int int int) Caqti_type.int
      {eos|
        select card_id
        from game_cards
        where game_id = ?
        and idx >= ?
        order by idx asc
        limit ?
      |eos}

  let find_board_cards_query =
    Caqti_request.collect Caqti_type.int Caqti_type.int
      {eos|
        select card_id
        from board_cards
        where game_id = ?
        order by idx asc;
      |eos}

  let find_player_score_query =
    Caqti_request.collect Caqti_type.int Caqti_type.(tup2 int int)
      {eos|
        select
            gp.player_id,
            (
                select count(*)
                from moves
                where moves.game_id = gp.game_id
                and moves.player_id = gp.player_id
            ) as score
        from games_players gp
        where game_id = ?;
      |eos}

    let update_player_name_query =
        Caqti_request.exec Caqti_type.(tup2 string int)
        {eos|
            update players
            set name = ?
            where id = ?
            limit 1;
        |eos}
end

let query_int (module Db : Caqti_lwt.CONNECTION) q =
  Db.find (Q.generic_int_query q) ()

let query_bool (module Db : Caqti_lwt.CONNECTION) q =
  Db.find (Q.generic_bool_query q) ()

let create_game (module Db : Caqti_lwt.CONNECTION) =
  Db.start () >>=? fun () ->
  Db.find Q.create_game_query () >>=? fun game_id ->
  Db.exec (Q.create_game_cards_query game_id) () >>=? fun () ->
  Db.exec Q.create_board_cards_query (game_id, board_size) >>=? fun () ->
  Db.find Q.increment_game_card_idx_query (board_size, game_id) >>=? fun _ ->
  Db.commit () >>=? fun () ->
  Lwt.return_ok game_id

let game_exists (module Db : Caqti_lwt.CONNECTION) game_id =
  Db.find Q.game_exists_query game_id

let create_player (module Db : Caqti_lwt.CONNECTION) =
  Db.find Q.create_player_query ()

let player_exists (module Db : Caqti_lwt.CONNECTION) player_id =
  Db.find Q.player_exists_query player_id

let game_player_presence (module Db : Caqti_lwt.CONNECTION) game_id player_id present =
  Db.exec Q.game_player_presence_query (game_id, player_id, present)

let find_board_cards (module Db : Caqti_lwt.CONNECTION) game_id =
  Db.collect_list Q.find_board_cards_query game_id

let create_move (module Db : Caqti_lwt.CONNECTION) game_id player_id idx0 card0_id idx1 card1_id idx2 card2_id =
  Db.start () >>=? fun () ->
  Db.exec (Q.set_transaction_mode_query "serializable") () >>=? fun () ->
  Db.exec Q.create_move_query (game_id, (player_id, (idx0, (card0_id, (idx1, (card1_id, (idx2, card2_id))))))) >>=? fun () ->
  Db.find Q.increment_game_card_idx_query (3, game_id) >>=? fun card_idx ->
  Db.collect_list Q.find_game_cards_query (game_id, card_idx, 3) >>=? fun card_ids_list ->
  let card_ids = Array.of_list card_ids_list in
  Db.find Q.update_board_card_query (idx0, (card0_id, (card_ids.(0), (idx1, (card1_id, (card_ids.(1), (idx2, (card2_id, (card_ids.(2), game_id))))))))) >>=? fun num_updated ->
  if num_updated != 3 then Db.rollback () else Db.commit ()

let find_player_score (module Db : Caqti_lwt.CONNECTION) game_id =
  Db.collect_list Q.find_player_score_query game_id

let update_player_name (module Db : Caqti_lwt.CONNECTION) player_id name =
  Db.exec Q.update_player_name_query (name, player_id)

let delete_all (module Db : Caqti_lwt.CONNECTION) =
  Db.start () >>=? fun () ->
  Db.exec (Q.generic_exec_query "truncate table games cascade;") () >>=? fun () ->
  Db.exec (Q.generic_exec_query "truncate table players cascade;") () >>=? fun () ->
  Db.exec (Q.generic_exec_query "truncate table games_players cascade;") () >>=? fun () ->
  Db.exec (Q.generic_exec_query "truncate table game_cards cascade;") () >>=? fun () ->
  Db.exec (Q.generic_exec_query "truncate table board_cards cascade;") () >>=? fun () ->
  Db.exec (Q.generic_exec_query "truncate table moves cascade;") () >>=? fun () ->
  Db.commit ()
