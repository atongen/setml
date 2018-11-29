open Cow

let asset_path ~manifest asset =
    let path = match List.assoc_opt asset manifest with
    | Some value -> value
    | None -> asset
    in
    Uri.of_string ("/assets/" ^ path)

let page_tpl ~player_id ~page_title ~token ~manifest ~assets id =
    let pid = match player_id with
    | Some pid -> string_of_int pid
    | None -> "" in
    Html.(
        html (list [
            head (list [
                title (string page_title);
                meta ~name: "viewport" ~content: "width=device-width, initial-scale=1, user-scalable=0, maximum-scale=1, minimum-scale=1" [];
                meta ~charset:"UTF-8" [];
                meta ~name:"token" ~content:token [];
                meta ~name:"player_id" ~content:pid [];
                link ~rel:"stylesheet" (Uri.of_string "https://fonts.googleapis.com/css?family=Roboto:300,400,500");
                link ~rel:"stylesheet" (Uri.of_string "https://fonts.googleapis.com/icon?family=Material+Icons");
                link ~rel:"stylesheet" (asset_path ~manifest "style.css" );
            ]);

            body (list (List.append [
                div ~id:id empty;
                script ~src:(asset_path ~manifest "runtime.js") empty;
                script ~src:(asset_path ~manifest "vendors.js") empty;
            ] (List.map (fun asset ->
                script ~src:(asset_path ~manifest asset) empty;
            ) assets)))
        ])
    )

let index_page ~player_id ~title ~token ~manifest =
    page_tpl ~player_id ~page_title:title ~token ~manifest ~assets:["index.js"] "index"
    |> Html.to_string

let game_page ~player_id ~title ~token ~manifest =
    page_tpl ~player_id ~page_title:title ~token ~manifest ~assets:["path_data.js"; "game.js"] "game"
    |> Html.to_string

let error_tpl msg =
    Html.(
        html (list [
            head (list [
                title (string "Error!");
                meta ~name: "viewport" ~content: "width=device-width, initial-scale=1, user-scalable=0, maximum-scale=1, minimum-scale=1" [];
                meta ~charset:"UTF-8" [];
            ]);

            body (list [
                h1 (string ("Error!"));
                p (string msg)
            ])
        ])
    )

let error msg = error_tpl msg |> Html.to_string
