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

let create_game_query =
  Caqti_request.find Caqti_type.unit Caqti_type.int
    "insert into games default values returning id"

let create_game_cards_query game_id =
  let cards = Game_deck.make 12 (-1) in
  let ids = List.map Card.to_int cards in
  let rows = List.mapi (fun idx card_id ->
      "(" ^ string_of_int game_id ^ ", " ^ string_of_int idx ^ ", " ^ string_of_int card_id ^ ")"
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
  Caqti_request.find args8 Caqti_type.(tup3 int int int)
    {eos|
        insert into moves (
            game_id,
            ?, -- player_id
            idx0,
            card0_id,
            idx1,
            card1_id,
            idx2,
            card2_id,
        )
        select
            bc0.game_id as game_id,
            bc0.idx as idx0,
            bc0.card_id as card0_id,
            bc1.idx as idx1,
            bc1.card_id as card1_id,
            bc2.idx as idx2,
            bc2.card_id as card2_id,
        from board_cards as bc0
            where bc0.game_id = ?
            and bc0.idx = ?
            and bc0.card_id = ?
        inner join board_cards as bc1
            where bc1.game_id = bc0.game_id
            and bc1.idx = ?
            and bc1.card_id = ?
        inner join board_cards as bc2
            where bc2.game_id = bc0.game_id
            and bc2.idx = ?
            and bc2.card_id = ?
        returning (
            bc0.id,
            bc1.id,
            bc2.id
        );
    |eos}

let update_board_card_query =
  let args6 = Caqti_type.(let (&) = tup2 in int & int & int & int & int & int) in
  Caqti_request.exec args6
    {eos|
        update board_cards as bc set
        card_id = c.card_id
        from (values
            (?, ?),
            (?, ?),
            (?, ?)
        ) as c(id, card_id)
        where c.id = bc.id;
    |eos}

let increment_game_card_idx_query =
    Caqti_request.find Caqti_type.(tup2 int int) Caqti_type.int
    {eos|
        update games
        set card_idx = card_idx + ?
        where game_id = ?
        returning card_idx
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
  Db.find create_move_query (player_id, (game_id, (idx0, (card0_id, (idx1, (card1_id, (idx2, card2_id))))))) >>=? fun (bc0_id, bc1_id, bc2_id) ->
  Db.find increment_game_card_idx_query (game_id, 3) >>=? fun card_idx ->
  Db.collect_list find_game_cards_query (game_id, card_idx, 3) >>=? fun card_ids_list ->
  let card_ids = Array.of_list card_ids_list in
  Db.exec update_board_card_query (card_ids.(0), (bc0_id, (card_ids.(1), (bc1_id, (card_ids.(2), bc2_id))))) >>=? fun () ->
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

let delete_all (module Db : Caqti_lwt.CONNECTION) =
    let make_query q = Caqti_request.exec Caqti_type.unit q in
    Db.start () >>=? fun () ->
    Db.exec (make_query "delete from games;") () >>=? fun () ->
    Db.exec (make_query "delete from players;") () >>=? fun () ->
    Db.commit ()
