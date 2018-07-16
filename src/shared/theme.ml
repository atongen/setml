type t =
    | Classic
    | Open_source

let to_string = function
    | Classic -> "classic"
    | Open_source -> "open_source"

let of_string = function
    | "classic" -> Classic
    | "open_source" -> Open_source
    | ts -> raise (Invalid_argument ("Unknown theme: " ^ ts))

let num theme card =
    match theme with
    | Classic | Open_source ->
        match Card.num card with
        | AttrZero -> "one"
        | AttrOne -> "two"
        | AttrTwo -> "three"

let fill theme card =
    match theme with
    | Classic | Open_source ->
        match Card.fill card with
        | AttrZero -> "open"
        | AttrOne -> "shaded"
        | AttrTwo -> "solid"

let color theme card =
    match theme with
    | Classic | Open_source ->
        match Card.color card with
        | AttrZero -> "red"
        | AttrOne -> "blue"
        | AttrTwo -> "green"

let shape ?(plural=false) theme card =
    match theme with
    | Classic -> (
        match Card.shape card with
        | AttrZero ->
            if plural then "ovals" else "oval"
        | AttrOne ->
            if plural then "diamonds" else "diamond"
        | AttrTwo ->
            if plural then "bowties" else "bowtie")
    | Open_source -> (
        match Card.shape card with
        | AttrZero ->
            if plural then "penguins" else "penguin"
        | AttrOne ->
            if plural then "elephants" else "elephant"
        | AttrTwo ->
            if plural then "camels" else "camel")

let card_to_string theme card =
    let num_attr = num theme card in
    let fill_attr = fill theme card in
    let color_attr = color theme card in
    let plural = card.num != AttrZero in
    let shape_attr = shape ~plural theme card in
    Printf.sprintf "%s %s %s %s" num_attr fill_attr color_attr shape_attr
