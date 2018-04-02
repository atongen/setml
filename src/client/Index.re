let token = () => {
    let rec aux = (c) => {
        switch (c) {
        | [] => ""
        | hd :: tl => if String.compare (Webapi.Dom.Element.getAttribute("name", hd), "token") = 0 then
                Webapi.Dom.Element.getAttribute("content", hd);
            else
                aux (tl);

        }
    };
    aux (DocumentRe.getElementsByTagName("meta", Webapi.Dom.document));
};

ReactDOMRe.renderToElementWithId(<Page message="Hello!" />, "index");
