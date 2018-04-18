open Lwt.Infix
open Shared

let (>>=?) m f =
  m >>= (function | Ok x -> f x | Error err -> Lwt.return (Error err))

let create_game_query =
  Caqti_request.find Caqti_type.unit Caqti_type.int
    "insert into games default values returning id"

let create_game (module Db : Caqti_lwt.CONNECTION) =
  Db.find create_game_query () >>=? fun game_id ->
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

let create_game_cards_query game_id =
  let cards = Game_deck.make 12 (-1) in
  let ids = List.map Card.to_int cards in
  let rows = List.mapi (fun idx card_id ->
      "(" ^ string_of_int game_id ^ ", " ^ string_of_int idx ^ ", " ^ string_of_int card_id ^ ")"
    ) ids in
  let query_values = String.concat ", " rows in
  let query = "insert into game_cards (game_id, idx, card_id) values " ^ query_values ^ ";" in
  Caqti_request.exec Caqti_type.unit query

let create_game_cards (module Db : Caqti_lwt.CONNECTION) game_id =
  let query = create_game_cards_query game_id in
  Db.exec query ()

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

let create_board_cards (module Db : Caqti_lwt.CONNECTION) game_id =
  Db.exec create_board_cards_query game_id

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
  Caqti_request.exec Caqti_type.(tup2 int int)
    {eos|
        update board_cards
        set card_id = ?
        where id = ?
    |eos}

(* within transaction (Db.start)
   insert `moves` row
   update `board_cards` with values from next `game_cards`
   increment `games`.`card_idx` *)

let create_move (module Db : Caqti_lwt.CONNECTION) game_id player_id idx0 card0_id idx1 card1_id idx2 card2_id =
  Db.start () >>=? fun () ->
  Db.find create_move_query (player_id, (game_id, (idx0, (card0_id, (idx1, (card1_id, (idx2, card2_id))))))) >>=? fun (bc0_id, bc1_id, bc2_id) ->
  Db.exec update_board_card_query (1, bc0_id) >>=? fun () ->
  Db.exec update_board_card_query (1, bc1_id) >>=? fun () ->
  Db.exec update_board_card_query (1, bc2_id) >>=? fun () ->
  Db.commit ()
