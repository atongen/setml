open Cow

let asset_path asset manifest =
    let path = match List.assoc_opt asset manifest with
    | Some value -> value
    | None -> asset
    in
    Uri.of_string ("/assets/" ^ path)

let page_tpl ~player_id id page_title token manifest =
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
                link ~rel:"stylesheet" (asset_path "style.css" manifest);
            ]);

            body (list [
                div ~id:id empty;
                script ~src:(asset_path "runtime.js" manifest) empty;
                script ~src:(asset_path "vendors.js" manifest) empty;
                script ~src:(asset_path "path_data.js" manifest) empty;
                script ~src:(asset_path (id ^ ".js") manifest) empty;
            ])
        ])
    )

let page ~player_id id title token manifest =
    page_tpl ~player_id id title token manifest
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
