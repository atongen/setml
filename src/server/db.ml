open Lwt.Infix
open Shared

type t = (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t

let (>>=?) m f =
    m >>= function
    | Ok x -> f x
    | Error err -> (Lwt.return_error err)

type mode =
    | ReadUncommitted
    | ReadCommitted
    | RepeatableRead
    | Serializable

let string_of_mode = function
    | ReadUncommitted -> "READ UNCOMMITTED"
    | ReadCommitted -> "READ COMMITTED"
    | RepeatableRead -> "REPEATABLE READ"
    | Serializable -> "SERIALIZABLE"

module Q = struct
    let set_transaction_mode_query mode =
        Caqti_request.exec ~oneshot:true Caqti_type.unit (Printf.sprintf "set transaction isolation level %s;" mode)

    let test_query = Caqti_request.exec Caqti_type.unit "select 1;"

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

    let find_player_data_query =
        Caqti_request.collect Caqti_type.int Caqti_type.(tup4 int string bool (tup2 int int))
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
                ) as score,
                (
                    select count(*)
                    from shuffles
                    where shuffles.game_id = gp.game_id
                    and shuffles.player_id = gp.player_id
                ) as shuffles
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

    let delete_games_query = Caqti_request.exec Caqti_type.unit "delete from games;"
    let delete_players_query = Caqti_request.exec Caqti_type.unit "delete from players;"
end


module I = struct
    let test (module C : Caqti_lwt.CONNECTION) () =
        C.exec Q.test_query () >>=? fun () ->
        Lwt.return_ok ()

    let create_game (module C : Caqti_lwt.CONNECTION) () =
        C.find Q.create_game_query () >>=? fun game_id ->
        C.find (Q.create_game_cards_query game_id) () >>=? fun _ ->
        C.exec Q.create_board_cards_query (game_id, 12) >>=? fun () ->
        C.find Q.increment_game_card_idx_query (12, game_id) >>=? fun _ ->
        Lwt.return_ok game_id

    let game_exists (module C : Caqti_lwt.CONNECTION) game_id =
        C.find Q.game_exists_query game_id >>=? fun exists ->
        Lwt.return_ok exists

    let create_player (module C : Caqti_lwt.CONNECTION) () =
        C.find Q.create_player_query () >>=? fun player_id ->
        Lwt.return_ok player_id

    let player_exists (module C : Caqti_lwt.CONNECTION) player_id =
        C.find Q.player_exists_query player_id >>=? fun exists ->
        Lwt.return_ok exists

    let game_player_presence (module C : Caqti_lwt.CONNECTION) args =
        C.exec Q.game_player_presence_query args >>=? fun () ->
        Lwt.return_ok ()

    let increment_game_card_idx (module C : Caqti_lwt.CONNECTION) (game_id, offset) =
        C.find Q.increment_game_card_idx_query (offset, game_id) >>=? fun card_idx ->
        Lwt.return_ok card_idx

    let create_move (module C : Caqti_lwt.CONNECTION) (game_id, player_id, idx0, card0, idx1, card1, idx2, card2) =
        let card0_id, card1_id, card2_id = (Card.to_int card0, Card.to_int card1, Card.to_int card2) in
        C.exec (Q.set_transaction_mode_query "serializable") () >>=? fun () ->
        C.exec Q.create_move_query (game_id, (player_id, (idx0, (card0_id, (idx1, (card1_id, (idx2, card2_id))))))) >>=? fun () ->
        C.find Q.find_game_card_idx_query game_id >>=? fun deck_card_idx ->
        C.collect_list Q.find_game_cards_query (game_id, deck_card_idx, 3) >>=? fun cards_list ->
        let card_ids = List.map (fun (_, card_id) -> card_id) cards_list |> Array.of_list in
        let new_card_0_id = Server_util.get_or card_ids 0 81 in
        let new_card_1_id = Server_util.get_or card_ids 1 81 in
        let new_card_2_id = Server_util.get_or card_ids 2 81 in
        C.find Q.update_board_cards_query ((idx0, card0_id, new_card_0_id), (idx1, card1_id, new_card_1_id), (idx2, card2_id, new_card_2_id), game_id) >>=? fun num_updated ->
        if num_updated != 3 then
            Lwt.return_error "3 board cards were not updated"
        else
            C.find Q.increment_game_card_idx_query (3, game_id) >>=? fun card_idx ->
            Lwt.return_ok card_idx

    let shuffle_board (module C : Caqti_lwt.CONNECTION) (game_id, player_id) =
        C.exec (Q.set_transaction_mode_query "serializable") () >>=? fun () ->
        C.find Q.find_game_card_idx_query game_id >>=? fun deck_card_idx ->
        (* include board in deck *)
        let card_idx = deck_card_idx - 12 in

        (* ensure we have at least board size 12 + 3 cards left to swap *)
        if card_idx < 0 || deck_card_idx >= 81 then
            Lwt.return_error "invalid game card index for shuffle"
        else

        (* get a list of all (idx, card_id) remaining for this game (including the board) *)
        C.collect_list Q.find_game_cards_query (game_id, 0, 81) >>=? fun cards_list ->

        (* get a list of all (idx, card_id) for cards currently on the board *)
        C.collect_list Q.find_random_board_cards_query (game_id, 3) >>=? fun random_board_cards ->
        if List.length random_board_cards < 3 then
            Lwt.return_error "unable to get at least 3 random cards from board"
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

        if List.length to_delete_list < 3 then
            Lwt.return_error "unable to find 3 random cards to delete from board"
        else

        let to_delete = Array.of_list to_delete_list in

        C.find Q.delete_game_cards_for_shuffle (game_id, (to_delete.(0), (to_delete.(1), (to_delete.(2), deck_card_idx)))) >>=? fun num_deleted ->

        let num_should_delete = 3 + 81 - deck_card_idx in
        if num_deleted < num_should_delete then
            Lwt.return_error "number of cards deleted for shuffle was less than required"
        else

        let deck = CCList.drop deck_card_idx cards_list in

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

        let num_should_added = 3 + 81 - deck_card_idx in
        if num_added < num_should_added then
            Lwt.return_error "number of cards added for shuffle was less than required"
        else

        C.collect_list Q.find_board_cards_query game_id >>=? fun board_card_ids ->
        let sets_on_board = Card.of_int_list board_card_ids
            |> Shared_util.compact
            |> Card.count_sets in

        let board_update = List.map2 (fun (idx, old_card_id) (_, new_card_id) ->
            (idx, old_card_id, new_card_id)
        ) random_board_cards new_board_cards |> Array.of_list in

        C.find Q.update_board_cards_query (board_update.(0), board_update.(1), board_update.(2), game_id) >>=? fun num_updated ->
        if num_updated < 3 then
            Lwt.return_error "less than 3 cards updated on board after shuffle"
        else

        C.exec Q.create_shuffle_query (game_id, player_id, sets_on_board) >>=? fun () ->
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
        C.exec Q.update_player_name_query (name, player_id) >>=? fun () ->
        Lwt.return_ok ()

    let find_board_cards (module C : Caqti_lwt.CONNECTION) game_id =
        C.collect_list Q.find_board_cards_query game_id >>=? fun result ->
        Lwt.return_ok result

    let find_game_cards (module C : Caqti_lwt.CONNECTION) (game_id, offset) =
        C.collect_list Q.find_game_cards_query (game_id, offset, 81 - offset) >>=? fun result ->
        Lwt.return_ok result

    let find_player_data (module C : Caqti_lwt.CONNECTION) game_id =
        C.collect_list Q.find_player_data_query game_id >>=? fun player_data ->
        let open Messages in
        Lwt.return_ok (List.map (fun (player_id, name, presence, (score, shuffles)) ->
            make_player_data player_id name presence score shuffles
        ) player_data)

    let delete_all (module C : Caqti_lwt.CONNECTION) () =
        C.exec Q.delete_games_query () >>=? fun () ->
        C.exec Q.delete_players_query () >>=? fun () ->
        Lwt.return_ok ()
end

let with_transaction ?(mode=ReadCommitted) (module C : Caqti_lwt.CONNECTION) f arg =
    C.start () >>=? fun () ->
    C.exec (Q.set_transaction_mode_query (string_of_mode mode)) () >>=? fun () ->
    f (module C : Caqti_lwt.CONNECTION) arg >>= function
    | Ok a ->
        C.commit () >>=? fun () ->
        Lwt.return_ok a
    | Error e ->
        C.rollback () >>=? fun () ->
        Lwt.return_error e


let with_pool ?(mode=ReadCommitted) (pool: t) f arg =
    Caqti_lwt.Pool.use (fun (module C : Caqti_lwt.CONNECTION) ->
        with_transaction ~mode (module C : Caqti_lwt.CONNECTION) f arg
    ) pool

let create_game p arg = with_pool p I.create_game arg

let game_exists p arg = with_pool p I.game_exists arg

let create_player p arg = with_pool p I.create_player arg

let player_exists p arg = with_pool p I.player_exists arg

let game_player_presence p arg = with_pool p I.game_player_presence arg

let increment_game_card_idx p arg = with_pool p I.increment_game_card_idx arg

let create_move p arg = with_pool ~mode:Serializable p I.create_move arg

let shuffle_board p arg = with_pool ~mode:Serializable p I.shuffle_board arg

let is_game_over p arg = with_pool p I.is_game_over arg

let update_player_name p arg = with_pool p I.update_player_name arg

let find_board_cards p arg = with_pool p I.find_board_cards arg

let find_game_cards p arg = with_pool p I.find_game_cards arg

let find_player_data p arg = with_pool p I.find_player_data arg

let delete_all p arg = with_pool p I.delete_all arg
