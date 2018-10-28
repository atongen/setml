type t =
    | Classic
    | Open_source

type palette = {
    primary: string;
    secondary: string;
    tertiary: string;
}

let to_string = function
    | Classic -> "classic"
    | Open_source -> "open_source"

let of_string = function
    | "classic" -> Classic
    | "open_source" -> Open_source
    | ts -> raise (Invalid_argument ("Unknown theme: " ^ ts))

let num ~card = function
    | Classic | Open_source ->
        match Card.num card with
        | AttrZero -> "one"
        | AttrOne -> "two"
        | AttrTwo -> "three"

let fill ~card = function
    | Classic | Open_source ->
        match Card.fill card with
        | AttrZero -> "open"
        | AttrOne -> "shaded"
        | AttrTwo -> "solid"

let color ~card = function
    | Classic | Open_source ->
        match Card.color card with
        | AttrZero -> "red"
        | AttrOne -> "blue"
        | AttrTwo -> "green"

let shape ?(plural=false) ~card = function
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

let palette = function
    | Classic -> {
        primary = "#3f51b5";
        secondary = "#f44336";
        tertiary = "#899f51";
    }
    | Open_source -> {
        primary = "";
        secondary = "";
        tertiary = "";
    }

let card_to_string ~theme card =
    let num_attr = num ~card theme in
    let fill_attr = fill ~card theme in
    let color_attr = color ~card theme in
    let plural = card.num != AttrZero in
    let shape_attr = shape ~plural ~card theme in
    Printf.sprintf "%s %s %s %s" num_attr fill_attr color_attr shape_attr
