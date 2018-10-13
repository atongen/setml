open Lwt.Infix
open Shared

type err =
    | Server_error of Caqti_error.t
    | Client_error of string

type t = (Caqti_lwt.connection, Caqti_error.t) Caqti_lwt.Pool.t

let (>>=?) m f =
    m >>= function
    | Ok x -> f x
    | Error e -> (Lwt.return_error (Server_error e))

let (>>=*) m f =
    m >>= function
    | Ok x -> f x
    | Error e -> (Lwt.return_error e)

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
        Caqti_request.find Caqti_type.(tup2 int int) Caqti_type.int
        "insert into games (dim0, dim1) values (?, ?) returning id"

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

    let set_game_player_presence_query =
        Caqti_request.exec Caqti_type.(tup3 int int bool)
        {eos|
            insert into games_players (game_id, player_id, presence, updated_at)
            values (?, ?, ?, now())
            on conflict (game_id, player_id)
            do update set presence = excluded.presence,
            updated_at = excluded.updated_at;
        |eos}

    let find_game_player_presence_query =
        Caqti_request.find Caqti_type.(tup2 int int) Caqti_type.bool
        {eos|
            select exists(
                select 1
                from games_players
                where game_id = ?
                and player_id = ?
                and presence = true
            )
        |eos}

    let create_move_query =
        let args = Caqti_type.(tup3 int int (tup3 (tup2 int int) (tup2 int int) (tup2 int int))) in
        Caqti_request.exec args
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

    let update_game_status_query =
        (* new_status game_id old_status *)
        Caqti_request.find Caqti_type.(tup3 string int string) Caqti_type.int
        {eos|
            with rows as (
                update games g
                set status = ?
                where g.id = ?
                and g.status = ?
                returning 1
            )
            select count(*) from rows;
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
        Caqti_request.collect Caqti_type.int Caqti_type.(tup2 int int)
        {eos|
            select idx, card_id
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

    let find_game_data_query =
        Caqti_request.find Caqti_type.int Caqti_type.(tup4 int string string (tup2 int int))
        {eos|
            select
                card_idx,
                status,
                theme,
                dim0,
                dim1
            from games
            where id = ?
        |eos}
end


module I = struct
    let create_game (module C : Caqti_lwt.CONNECTION) (dim0, dim1) =
        C.find Q.create_game_query (dim0, dim1) >>=? fun game_id ->
        C.find (Q.create_game_cards_query game_id) () >>=? fun _ ->
        C.exec Q.create_board_cards_query (game_id, 12) >>=? fun () ->
        C.find Q.increment_game_card_idx_query (12, game_id) >>=? fun _ ->
        Lwt.return_ok game_id

    let game_exists (module C : Caqti_lwt.CONNECTION) game_id =
        C.find Q.game_exists_query game_id >>=? fun exists ->
        Lwt.return_ok exists

    let find_game_card_idx (module C : Caqti_lwt.CONNECTION) game_id =
        C.find Q.find_game_card_idx_query game_id >>=? fun card_idx ->
        Lwt.return_ok card_idx

    let create_player (module C : Caqti_lwt.CONNECTION) () =
        C.find Q.create_player_query () >>=? fun player_id ->
        Lwt.return_ok player_id

    let player_exists (module C : Caqti_lwt.CONNECTION) player_id =
        C.find Q.player_exists_query player_id >>=? fun exists ->
        Lwt.return_ok exists

    let set_game_player_presence (module C : Caqti_lwt.CONNECTION) args =
        C.exec Q.set_game_player_presence_query args >>=? fun () ->
        Lwt.return_ok ()

    let find_game_player_presence (module C : Caqti_lwt.CONNECTION) args =
        C.find Q.find_game_player_presence_query args >>=? fun result ->
        Lwt.return_ok result

    let increment_game_card_idx (module C : Caqti_lwt.CONNECTION) (game_id, offset) =
        C.find Q.increment_game_card_idx_query (offset, game_id) >>=? fun card_idx ->
        Lwt.return_ok card_idx

    let create_move (module C : Caqti_lwt.CONNECTION) (game_id, player_id, ((cd0: Messages.card_data), (cd1: Messages.card_data), (cd2: Messages.card_data))) =
        if Card.is_set cd0.card cd1.card cd2.card then (
            let (idx0, card0_id, idx1, card1_id, idx2, card2_id) = (cd0.idx, Card.to_int cd0.card, cd1.idx, Card.to_int cd1.card, cd2.idx, Card.to_int cd2.card) in
            C.exec (Q.set_transaction_mode_query "serializable") () >>=? fun () ->
            C.exec Q.create_move_query (game_id, player_id, ((idx0, card0_id), (idx1, card1_id), (idx2, card2_id))) >>=? fun () ->
            C.find Q.find_game_card_idx_query game_id >>=? fun deck_card_idx ->
            C.collect_list Q.find_game_cards_query (game_id, deck_card_idx, 3) >>=? fun cards_list ->
            let card_ids = List.map (fun (_, card_id) -> card_id) cards_list |> Array.of_list in
            let new_card_0_id = Server_util.get_or card_ids 0 81 in
            let new_card_1_id = Server_util.get_or card_ids 1 81 in
            let new_card_2_id = Server_util.get_or card_ids 2 81 in
            C.find Q.update_board_cards_query ((idx0, card0_id, new_card_0_id), (idx1, card1_id, new_card_1_id), (idx2, card2_id, new_card_2_id), game_id) >>=? fun num_updated ->
            if num_updated != 3 then
                Lwt.return_error (Client_error "3 board cards were not updated")
            else if deck_card_idx <= 78 then
                C.find Q.increment_game_card_idx_query (3, game_id) >>=? fun card_idx ->
                Lwt.return_ok card_idx
            else
                Lwt.return_ok deck_card_idx
        ) else Lwt.return_error (Client_error "board cards are not a set")

    let create_shuffle (module C : Caqti_lwt.CONNECTION) (game_id, player_id) =
        C.exec (Q.set_transaction_mode_query "serializable") () >>=? fun () ->
        C.find Q.find_game_data_query game_id >>=? fun (deck_card_idx, _, _, (dim0, dim1)) ->
        let num_board_cards = dim0 * dim1 in
        (* include board in deck *)
        let card_idx = deck_card_idx - num_board_cards in

        if card_idx < 0 || deck_card_idx >= 81 then
            Lwt.return_ok false
        else

        (* get a list of all (idx, card_id) remaining for this game (including the board) *)
        C.collect_list Q.find_game_cards_query (game_id, 0, 81) >>=? fun cards_list ->

        (* get a list of all (idx, card_id) for cards currently on the board *)
        C.collect_list Q.find_random_board_cards_query (game_id, 3) >>=? fun random_board_cards ->
        if List.length random_board_cards < 3 then
            Lwt.return_error (Client_error "unable to get at least 3 random cards from board")
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
            Lwt.return_error (Client_error "unable to find 3 random cards to delete from board")
        else

        let to_delete = Array.of_list to_delete_list in

        C.find Q.delete_game_cards_for_shuffle (game_id, (to_delete.(0), (to_delete.(1), (to_delete.(2), deck_card_idx)))) >>=? fun num_deleted ->

        let num_should_delete = 3 + 81 - deck_card_idx in
        if num_deleted < num_should_delete then
            Lwt.return_error (Client_error "number of cards deleted for shuffle was less than required")
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
            Lwt.return_error (Client_error "number of cards added for shuffle was less than required")
        else

        C.collect_list Q.find_board_cards_query game_id >>=? fun board_cards ->
        let sets_on_board = List.map (fun (idx, _) -> idx) board_cards
            |> Card.of_int_list
            |> Shared_util.compact
            |> Card.count_sets in

        let board_update = List.map2 (fun (idx, old_card_id) (_, new_card_id) ->
            (idx, old_card_id, new_card_id)
        ) random_board_cards new_board_cards |> Array.of_list in

        C.find Q.update_board_cards_query (board_update.(0), board_update.(1), board_update.(2), game_id) >>=? fun num_updated ->
        if num_updated < 3 then
            Lwt.return_error (Client_error "less than 3 cards updated on board after shuffle")
        else

        C.exec Q.create_shuffle_query (game_id, player_id, sets_on_board) >>=? fun () ->
        Lwt.return_ok true

    let is_game_over (module C : Caqti_lwt.CONNECTION) game_id =
        let set_exists_in_cards cards =
            List.map (fun (_, card_id) -> card_id) cards
                |> Card.of_int_list
                |> Shared_util.compact
                |> Card.exists_set
        in
        C.find Q.find_game_card_idx_query game_id >>=? fun card_idx ->
        C.collect_list Q.find_board_cards_query game_id >>=? fun board_cards ->
        C.collect_list Q.find_game_cards_query (game_id, card_idx, 81 - card_idx) >>=? fun deck_cards ->
        let cards = List.concat [board_cards; deck_cards] in
        Lwt.return_ok (not (set_exists_in_cards cards))

    let start_game (module C : Caqti_lwt.CONNECTION) game_id =
        let old_status = Game_status.to_string Game_status.New in
        let new_status = Game_status.to_string Game_status.Started in
        C.find Q.update_game_status_query (new_status, game_id, old_status) >>=? fun num_updated ->
        if num_updated < 1 then
            Lwt.return_error (Client_error "unable to transition game status to 'started'")
        else
            Lwt.return_ok ()

    let end_game (module C : Caqti_lwt.CONNECTION) game_id =
        let old_status = Game_status.to_string Game_status.Started in
        let new_status = Game_status.to_string Game_status.Complete in
        C.find Q.update_game_status_query (new_status, game_id, old_status) >>=? fun num_updated ->
        if num_updated < 1 then
            Lwt.return_error (Client_error "unable to transition game status to 'complete'")
        else
            Lwt.return_ok ()

    let update_player_name (module C : Caqti_lwt.CONNECTION) (player_id, name) =
        C.exec Q.update_player_name_query (name, player_id) >>=? fun () ->
        Lwt.return_ok ()

    let find_board_cards (module C : Caqti_lwt.CONNECTION) game_id =
        C.collect_list Q.find_board_cards_query game_id >>=? fun board_cards ->
        Lwt.return_ok (List.map (fun (idx, card_id) ->
            Shared.Messages.make_board_card_data idx card_id
        ) board_cards)

    let find_game_cards (module C : Caqti_lwt.CONNECTION) (game_id, offset) =
        C.collect_list Q.find_game_cards_query (game_id, offset, 81 - offset) >>=? fun game_cards ->
        Lwt.return_ok (List.map (fun (idx, card_id) ->
            Shared.Messages.make_card_data idx card_id
        ) game_cards)

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

    let find_game_data (module C : Caqti_lwt.CONNECTION) game_id =
        C.find Q.find_game_data_query game_id >>=? fun (card_idx, status, theme, (dim0, dim1)) ->
        let open Messages in
        Lwt.return_ok (make_game_update_data card_idx status theme dim0 dim1)
end

let with_transaction ?(mode=ReadCommitted) (module C : Caqti_lwt.CONNECTION) f arg =
    C.start () >>=* fun () ->
    C.exec (Q.set_transaction_mode_query (string_of_mode mode)) () >>=* fun () ->
    f (module C : Caqti_lwt.CONNECTION) arg >>= function
    | Ok a ->
        C.commit () >>=* fun () ->
        Lwt.return_ok a
    | Error e ->
        match e with
        | Server_error se ->
            C.rollback () >>=* fun () ->
            Lwt.return_error se
        | Client_error ce ->
            let r = Caqti_error.response_rejected ~query:"client error" ~uri:(Uri.of_string "") (Caqti_error.Msg ce) in
            C.rollback () >>=* fun () ->
            Lwt.return_error r


let with_pool ?(priority=0.0) ?(mode=ReadCommitted) (pool: t) f arg =
    Caqti_lwt.Pool.use ~priority (fun (module C : Caqti_lwt.CONNECTION) ->
        with_transaction ~mode (module C : Caqti_lwt.CONNECTION) f arg
    ) pool
let make ?(max_size=8) uri_str: (t, Caqti_error.t) result Lwt.t =
    Lwt.return (Caqti_lwt.connect_pool ~max_size (Uri.of_string uri_str))

let create_game p arg = with_pool p I.create_game arg

let game_exists p arg = with_pool p I.game_exists arg

let find_game_card_idx p arg = with_pool p I.find_game_card_idx arg

let create_player p arg = with_pool p I.create_player arg

let player_exists p arg = with_pool p I.player_exists arg

let set_game_player_presence p arg = with_pool p I.set_game_player_presence arg

let find_game_player_presence p arg = with_pool p I.find_game_player_presence arg

let increment_game_card_idx p arg = with_pool p I.increment_game_card_idx arg

let create_move p arg = with_pool ~priority:2.0 ~mode:Serializable p I.create_move arg

let create_shuffle p arg = with_pool ~priority:1.0 ~mode:Serializable p I.create_shuffle arg

let is_game_over p arg = with_pool p I.is_game_over arg

let start_game p arg = with_pool p I.start_game arg

let end_game p arg = with_pool p I.end_game arg

let update_player_name p arg = with_pool p I.update_player_name arg

let find_board_cards p arg = with_pool p I.find_board_cards arg

let find_game_cards p arg = with_pool p I.find_game_cards arg

let find_player_data p arg = with_pool p I.find_player_data arg

let delete_all p arg = with_pool p I.delete_all arg

let find_game_data p arg = with_pool p I.find_game_data arg
