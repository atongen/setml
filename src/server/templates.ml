open Cow

let game_tpl game_id =
    let game_id_str = Route.string_of_game_id game_id in
    Html.(
        html (list [
            head (list [
                title (string ("Game " ^ game_id_str));
                meta ~charset:"UTF-8" empty;
                script ~src:"/jquery.min.js" empty;
                script ~src:"/app.js" empty;
            ]);

            body (list [
                h1 (string ("SetML - Game " ^ game_id_str));
                p ~id:"target" (list [
                    string "Click me!"
                ]);
                div ~id:"msg" empty
            ])
        ])
    )

let game game_id = game_tpl game_id |> Html.to_string

let error_tpl msg =
    Html.(
        html (list [
            head (list [
                title (string "Error!");
                meta ~charset:"UTF-8" empty;
            ]);

            body (list [
                h1 (string ("Error!"));
                p (string msg)
            ])
        ])
    )

let error msg = error_tpl msg |> Html.to_string