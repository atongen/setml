open Belt;

let meta_content = name => {
  let rec aux = c =>
    switch (c) {
    | [] => ""
    | [hd, ...tl] =>
      let el_name = Webapi.Dom.Element.getAttribute("name", hd);
      switch (el_name) {
      | None => aux(tl)
      | Some(n) =>
        if (n == name) {
          let el_content = Webapi.Dom.Element.getAttribute("content", hd);
          switch (el_content) {
          | None => ""
          | Some(c) => c
          };
        } else {
          aux(tl);
        }
      };
    };
  DocumentRe.getElementsByTagName("meta", Webapi.Dom.document)
  |> Webapi.Dom.HtmlCollection.toArray
  |> List.fromArray
  |> aux;
};
