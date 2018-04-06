open Cow

let page_tpl id page_title token =
  Html.(
    html (list [
        head (list [
            title (string page_title);
            meta ~charset:"UTF-8" empty;
            meta ~name:"token" ~content:token empty;
            link ~rel:"stylesheet" ~href:(Uri.of_string "/css/style.css") empty;
          ]);

        body (list [
            div ~id:id empty;
            script ~src:("/js/" ^ id ^ ".js") empty;
          ])
      ])
  )

let page id title token =
  page_tpl id title token
  |> Html.to_string

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
