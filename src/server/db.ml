open Lwt.Infix
open Shared

let (>>=?) m f =
  m >>= function
    | Ok x -> f x
    | Error err -> Lwt.return (Error err)

(* used for testing to validate results of other queries *)
let query_int (module Db : Caqti_lwt.CONNECTION) q =
    let r = Caqti_request.find Caqti_type.unit Caqti_type.int q in
    Db.find r ()

let query_bool (module Db : Caqti_lwt.CONNECTION) q =
    let r = Caqti_request.find Caqti_type.unit Caqti_type.bool q in
    Db.find r ()

let create_game_query =
  Caqti_request.find Caqti_type.unit Caqti_type.int
    "insert into games default values returning id"

let create_game_cards_query game_id =
  let cards = Game_deck.make 12 (-1) in
  let ids = List.map Card.to_int cards in
  let rows = List.mapi (fun idx card_id ->
        Printf.sprintf "(%d,%d,%d)" game_id idx card_id
    ) ids in
  let query_values = String.concat ", " rows in
  let query = "insert into game_cards (game_id, idx, card_id) values " ^ query_values ^ ";" in
  Caqti_request.exec Caqti_type.unit query

let create_board_cards_query =
  Caqti_request.exec Caqti_type.int
    {eos|
    insert into board_cards (game_id, idx, card_id)
    select game_id, idx, card_id
    from game_cards
    where game_id = ?
    order by idx asc
    limit 12
    |eos}

let create_game (module Db : Caqti_lwt.CONNECTION) =
  Db.start () >>=? fun () ->
  Db.find create_game_query () >>=? fun game_id ->
  let query = create_game_cards_query game_id in
  Db.exec query () >>=? fun () ->
  Db.exec create_board_cards_query game_id >>=? fun () ->
  Db.commit () >>=? fun () ->
  Lwt.return_ok game_id

let game_exists_query =
  Caqti_request.find Caqti_type.int Caqti_type.bool
    "select exists (select 1 from games where id = ?)"

let game_exists (module Db : Caqti_lwt.CONNECTION) game_id =
  Db.find game_exists_query game_id

let create_player_query =
  Caqti_request.find Caqti_type.unit Caqti_type.int
    "insert into players default values returning id"

let create_player (module Db : Caqti_lwt.CONNECTION) =
  Db.find create_player_query ()

let player_exists_query =
  Caqti_request.find Caqti_type.int Caqti_type.bool
    "select exists (select 1 from players where id = ?)"

let player_exists (module Db : Caqti_lwt.CONNECTION) player_id =
  Db.find player_exists_query player_id

let game_player_present_query =
  Caqti_request.exec Caqti_type.(tup3 int int bool)
    {eos|
        insert into games_players (game_id, player_id, present, updated_at)
        values (?, ?, ?, now())
        on conflict (game_id, player_id)
        do update set present = excluded.present,
        updated_at = excluded.updated_at;
    |eos}

let game_player_presence (module Db : Caqti_lwt.CONNECTION) game_id player_id present =
  Db.exec game_player_present_query (game_id, player_id, present)

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
  Caqti_request.exec args10
    {eos|
        update board_cards as bc
        set card_id = moves.new_card_id::int
        from (values
            (?, ?, ?),
            (?, ?, ?),
            (?, ?, ?)
        ) as moves (
            idx,
            old_card_id,
            new_card_id
        )
        where bc.game_id = ?
            and bc.idx = moves.idx::int
            and bc.card_id = moves.old_card_id::int;
    |eos}

let increment_game_card_idx_query =
    Caqti_request.find Caqti_type.(tup2 int int) Caqti_type.int
    {eos|
        update games
        set card_idx = card_idx + ?
        where id = ?
        returning card_idx;
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

let create_move (module Db : Caqti_lwt.CONNECTION) game_id player_id idx0 card0_id idx1 card1_id idx2 card2_id =
  Db.start () >>=? fun () ->
  (*
   * set local transaction isolation serializable here
   * SET SESSION CHARACTERISTICS AS TRANSACTION SERIALIZABLE;
   *)
  Db.exec create_move_query (game_id, (player_id, (idx0, (card0_id, (idx1, (card1_id, (idx2, card2_id))))))) >>=? fun () ->
  Db.find increment_game_card_idx_query (3, game_id) >>=? fun card_idx ->
  Db.collect_list find_game_cards_query (game_id, card_idx, 3) >>=? fun card_ids_list ->
  let card_ids = Array.of_list card_ids_list in
  Db.exec update_board_card_query (game_id, (idx0, (card0_id, (card_ids.(0), (idx1, (card1_id, (card_ids.(1), (idx2, (card2_id, card_ids.(2)))))))))) >>=? fun () ->
  Db.commit ()

let find_board_cards_query =
    Caqti_request.collect Caqti_type.int Caqti_type.int
    {eos|
        select card_id
        from board_cards
        where game_id = ?
        order by idx asc;
    |eos}

let find_board_cards (module Db : Caqti_lwt.CONNECTION) game_id =
    Db.collect_list find_board_cards_query game_id

let find_player_score_query =
    Caqti_request.collect Caqti_type.int Caqti_type.(tup2 int int)
    {eos|
        select player_id, count(*)
        from moves
        where game_id = ?
        group by player_id;
    |eos}

let find_player_score (module Db : Caqti_lwt.CONNECTION) game_id =
    Db.collect_list find_player_score_query game_id

let delete_all (module Db : Caqti_lwt.CONNECTION) =
    let make_query q = Caqti_request.exec Caqti_type.unit q in
    Db.start () >>=? fun () ->
    Db.exec (make_query "delete from games;") () >>=? fun () ->
    Db.exec (make_query "delete from players;") () >>=? fun () ->
    Db.commit ()
