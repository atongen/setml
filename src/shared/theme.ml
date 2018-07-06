type t =
    | Classic

let to_string = function
    | Classic -> "classic"

let of_string = function
    | "classic" -> Classic
    | ts -> raise (Invalid_argument ("Unknown theme: " ^ ts))

let num theme card =
    match theme with
    | Classic ->
        match Card.num card with
        | AttrZero -> "one"
        | AttrOne -> "two"
        | AttrTwo -> "three"

let fill theme card =
    match theme with
    | Classic ->
        match Card.fill card with
        | AttrZero -> "open"
        | AttrOne -> "shaded"
        | AttrTwo -> "solid"

let color theme card =
    match theme with
    | Classic ->
        match Card.color card with
        | AttrZero -> "red"
        | AttrOne -> "blue"
        | AttrTwo -> "green"

let shape theme card =
    match theme with
    | Classic ->
        match Card.shape card with
        | AttrZero -> "oval"
        | AttrOne -> "diamond"
        | AttrTwo -> "bowtie"

let card_to_string theme card =
    let num_attr = num theme card in
    let fill_attr = fill theme card in
    let color_attr = color theme card in
    let shape_attr = shape theme card in
    Printf.sprintf "%s %s %s %s" num_attr fill_attr color_attr shape_attr
