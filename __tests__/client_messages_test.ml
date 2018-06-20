open Jest
open Expect

open Messages
open ClientMessages

let _ =
    let test_convert a =
        let open ClientMessageConverter in
        let b = a |> to_json |> of_json in
        expect a |> toEqual b
    in

    describe "ClientMessages" (fun () ->
        test "Game_data" (fun () ->
            test_convert (make_game_data [
                make_player_data 1 "john" true 5 1;
                make_player_data 2 "rich" true 4 2;
                make_player_data 3 "bill" false 6 3;
                make_player_data 4 "cindy" true 9 4;
                make_player_data 5 "carol" true 3 5;
            ] (Card.of_int_list [
                 0;  2;  4;  6;
                 8; 10; 33; 52;
                78; 79; 80; 81;
            ] |> Array.of_list)
            (make_game_update_data "started" 15))
        );

        test "Player_name" (fun () ->
            test_convert (make_player_name 6 "Billiam")
        );

        test "Board_card" (fun () ->
            test_convert (make_board_card 5 7)
        );

        test "Game_update new" (fun () ->
            test_convert (make_game_update "new" 5)
        );

        test "Game_update started" (fun () ->
            test_convert (make_game_update "started" 5)
        );

        test "Game_update complete" (fun () ->
            test_convert (make_game_update "complete" 5)
        );

        test "Score" (fun () ->
            test_convert (make_score 5 6)
        );

        test "Previous_move" (fun () ->
            test_convert (make_previous_move 11 13 15)
        );

        test "Player_presence" (fun () ->
            test_convert (make_player_presence 99 true)
        );

        test "Move_data" (fun () ->
            test_convert (make_move_data
                (make_score_data 23 25)
                (make_previous_move_data 1 2 3))
        );

        test "Shuffles" (fun () ->
            test_convert (make_shuffles 18 19)
        );

        test "Player_data" (fun () ->
            test_convert (Player_data (make_player_data 4 "wow" true 9 11))
        );
    )
