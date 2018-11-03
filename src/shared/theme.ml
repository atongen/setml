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
        | NumZero -> "one"
        | NumOne -> "two"
        | NumTwo -> "three"

let fill ~card = function
    | Classic | Open_source ->
        match Card.fill card with
        | FillZero -> "open"
        | FillOne -> "shaded"
        | FillTwo -> "solid"

let color ~card = function
    | Classic | Open_source ->
        match Card.color card with
        | ColorZero -> "red"
        | ColorOne -> "blue"
        | ColorTwo -> "green"

let shape ?(plural=false) ~card = function
    | Classic -> (
        match Card.shape card with
        | ShapeZero ->
            if plural then "ovals" else "oval"
        | ShapeOne ->
            if plural then "diamonds" else "diamond"
        | ShapeTwo ->
            if plural then "bowties" else "bowtie")
    | Open_source -> (
        match Card.shape card with
        | ShapeZero ->
            if plural then "penguins" else "penguin"
        | ShapeOne ->
            if plural then "elephants" else "elephant"
        | ShapeTwo ->
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
    let plural = card.num != NumZero in
    let shape_attr = shape ~plural ~card theme in
    Printf.sprintf "%s %s %s %s" num_attr fill_attr color_attr shape_attr

let default_card_size = (1000.0, 1000.0)

let make_svg ~width ~height ~vx ~vy ~vw ~vh content =
    Printf.sprintf
        {eosvg|
            <svg xmlns='http://www.w3.org/2000/svg' width='%f' height='%f' viewBox='%f %f %f %f'>
                %s
            </svg>
        |eosvg}
    width height vx vy vw vh content

module type CARD_SVG_THEME = sig
    val make_card_svgs : width:float -> height:float -> Card.t -> string list
end

module Card_svg_classic : CARD_SVG_THEME = struct
    let make_classic_shape_svg card =
        let color = match Card.color card with
        | ColorZero -> "red"
        | ColorOne -> "blue"
        | ColorTwo -> "green"
        in
        let (defs, fill) = match Card.fill card with
        | FillZero -> (* open *) ("", "fill=\"none\"")
        | FillOne -> (* shaded *)
            let defs = {eodefs|
                <defs>
                    <pattern id="a" patternTransform="scale(10)" height="1" width="2" patternUnits="userSpaceOnUse">
                        <path d="M0-.5h1v2H0z" />
                    </pattern>
                </defs>
            |eodefs}
            in
            let fill = "fill=\"url(#a)\"" in
            (defs, fill)
        | FillTwo -> (* *) ("", Printf.sprintf "fill=\"%s\"" color)
        in
        match Card.shape card with
        | ShapeZero -> ( (* oval *)
            Printf.sprintf
            {eoshape|
                %s
                <path
                    style="isolation:auto;mix-blend-mode:normal;solid-color:#000;solid-opacity:1"
                    d="M200 340A160 160 0 0 0 40 500a160 160 0 0 0 160 160h600a160 160 0 0 0 160-160 160 160 0 0 0-160-160H200z"
                    color="%s"
                    overflow="visible"
                    %s
                    stroke="%s"
                    stroke-width="10"
                />
            |eoshape}
            defs color fill color
        )
        | ShapeOne -> ( (* diamonds *)
            Printf.sprintf
            {eoshape|
                %s
                <circle
                    cx="500"
                    cy="500"
                    r="500"
                    color="%s"
                    overflow="visible"
                    %s
                    stroke="%s"
                    stroke-width="10"
                />
            |eoshape}
            defs color fill color
        )
        | ShapeTwo -> ( (* bowtie *)
            Printf.sprintf
            {eoshape|
                %s
                <circle
                    cx="500"
                    cy="500"
                    r="500"
                    color="%s"
                    overflow="visible"
                    %s
                    stroke="%s"
                    stroke-width="10"
                />
            |eoshape}
            defs color fill color
        )

    let make_card_svgs ~width ~height card =
        let content = make_classic_shape_svg card in
        let (vw, vh) = default_card_size in
        let svg = make_svg ~width ~height ~vx:0.0 ~vy:0.0 ~vw ~vh content in
        [svg]
end

let make_card_svgs ~width ~height ~theme card =
    match theme with
    | Classic | Open_source ->
        Card_svg_classic.make_card_svgs ~width ~height card
