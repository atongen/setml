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
        | FillZero -> (* open *) ("", "fill=\"white\"")
        | FillOne -> (* shaded *)
            let defs = Printf.sprintf
                {eodefs|
                    <defs>
                        <pattern id="a" height="18" width="18" patternUnits="userSpaceOnUse">
                            <rect width="18" height="18" fill="white" />
                            <rect x="0" y="0" width="4" height="18" fill="%s" />
                        </pattern>
                    </defs>
                |eodefs}
                color
            in
            let fill = "fill=\"url(#a)\"" in
            (defs, fill)
        | FillTwo -> (* solid *) ("", Printf.sprintf "fill=\"%s\"" color)
        in
        match Card.shape card with
        | ShapeZero -> ( (* oval *)
            let path_d = match Card.num card with
            | NumZero -> "M400 400a100 100 0 0 0-100 100 100 100 0 0 0 100 100h200a100 100 0 0 0 100-100 100 100 0 0 0-100-100H400z"
            | NumOne -> "M400 550a100 100 0 0 0-100 100 100 100 0 0 0 100 100h200a100 100 0 0 0 100-100 100 100 0 0 0-100-100H400zM400 250a100 100 0 0 0-100 100 100 100 0 0 0 100 100h200a100 100 0 0 0 100-100 100 100 0 0 0-100-100H400z"
            | NumTwo -> "M400 100a100 100 0 0 0-100 100 100 100 0 0 0 100 100h200a100 100 0 0 0 100-100 100 100 0 0 0-100-100H400zM400 400a100 100 0 0 0-100 100 100 100 0 0 0 100 100h200a100 100 0 0 0 100-100 100 100 0 0 0-100-100H400zM400 700a100 100 0 0 0-100 100 100 100 0 0 0 100 100h200a100 100 0 0 0 100-100 100 100 0 0 0-100-100H400z"
            in
            Printf.sprintf
            {eoshape|
                %s
                <g
                    %s
                    color="%s"
                    stroke="%s"
                    stroke-width="10"
                    stroke-linejoin="round">
                    <path d="%s"/>
                </g>
            |eoshape}
            defs fill color color path_d
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
        let (vw, vh) = default_card_size in
        let content = make_classic_shape_svg card in
        [make_svg ~width ~height ~vx:0.0 ~vy:0.0 ~vw ~vh content]
end

let make_card_svgs ~width ~height ~theme card =
    match theme with
    | Classic | Open_source ->
        Card_svg_classic.make_card_svgs ~width ~height card
