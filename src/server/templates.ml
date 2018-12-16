open Cow

let asset_path ~manifest asset =
    let path = match List.assoc_opt asset manifest with
    | Some value -> value
    | None -> asset
    in
    Uri.of_string ("/assets/" ^ path)

let build_meta_list ~seed content =
    let entry key value =
        if key = "charset" then
            Html.meta ~charset:value []
        else
            Html.meta ~name:key ~content:value []
    in
    let rec aux acc = function
    | [] -> acc
    | (k,v) :: tl -> match v with
        | Some(value) -> if value != "" then
            aux ((entry k value) :: acc) tl
            else aux acc tl
        | None -> aux acc tl
    in
    List.append (aux [] (List.rev seed))
                (aux [] (List.rev content))

let page_tpl ~page_title ~manifest ~assets ~meta id =
    let meta_list = build_meta_list ~seed:[
        ("viewport", Some("width=device-width, initial-scale=1, user-scalable=0, maximum-scale=1, minimum-scale=1, shrink-to-fit=no"));
        ("charset", Some("UTF-8"));
    ] meta
    in
    Html.(
        html (list [
            head (list (List.append [
                title (string page_title);
                link ~rel:"stylesheet" (Uri.of_string "https://fonts.googleapis.com/css?family=Roboto:300,400,500");
                link ~rel:"stylesheet" (Uri.of_string "https://fonts.googleapis.com/icon?family=Material+Icons");
            ] meta_list));

            body ~cls:"mat-typography" ~attrs:[("style", "padding: 0; margin: 0")] (list (List.append [
                div ~id:id empty;
                script ~src:(asset_path ~manifest "runtime.js") empty;
                script ~src:(asset_path ~manifest "vendors.js") empty;
            ] (List.map (fun asset ->
                script ~src:(asset_path ~manifest asset) empty;
            ) assets)))
        ])
    )

let pid = function
    | Some(id) -> Some(string_of_int id)
    | None -> None

let index_page ~title ~player_id ~token ~manifest ~info =
    let meta = [
        ("player_id", pid player_id);
        ("token", Some token);
        ("info", Some info);
    ] in
    page_tpl ~page_title:title ~manifest ~assets:["index.js"] ~meta "index"
    |> Html.to_string

let game_page ~title ~player_id ~token ~manifest ~info =
    let meta = [
        ("player_id", pid player_id);
        ("token", Some token);
        ("info", Some info);
    ] in
    page_tpl ~page_title:title ~manifest ~assets:["path_data.js"; "game.js"] ~meta "game"
    |> Html.to_string

let error_tpl msg =
    Html.(
        html (list [
            head (list [
                title (string "Error!");
                meta ~name: "viewport" ~content: "width=device-width, initial-scale=1, user-scalable=0, maximum-scale=1, minimum-scale=1" [];
                meta ~charset:"UTF-8" [];
            ]);

            body ~cls:"mat-typography" (list [
                h1 (string ("Error!"));
                p (string msg)
            ])
        ])
    )

let error msg = error_tpl msg |> Html.to_string
