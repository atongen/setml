open Cow

let page_tpl ~player_id id page_title token =
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
                link ~rel:"stylesheet" (Uri.of_string "/css/style.css");
            ]);

            body (list [
                div ~id:id empty;
                script ~src:(Uri.of_string ("/js/" ^ id ^ ".js")) empty;
            ])
        ])
    )

let page ?player_id id title token =
    page_tpl ~player_id id title token
    |> Html.to_string

let error_tpl msg =
    Html.(
        html (list [
            head (list [
                title (string "Error!");
                meta ~name: "viewport" ~content: "width=device-width, initial-scale=1, user-scalable=0, maximum-scale=1, minimum-scale=1" [];
                meta ~charset:"UTF-8" [];
                link ~rel:"stylesheet" (Uri.of_string "https://fonts.googleapis.com/css?family=Roboto:300,400,500");
                link ~rel:"stylesheet" (Uri.of_string "https://fonts.googleapis.com/icon?family=Material+Icons");
                link ~rel:"stylesheet" (Uri.of_string "/css/style.css");
            ]);

            body (list [
                h1 (string ("Error!"));
                p (string msg)
            ])
        ])
    )

let error msg = error_tpl msg |> Html.to_string
