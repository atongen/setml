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
    | Classic -> (
        match Card.color card with
        | ColorZero -> "red"
        | ColorOne -> "purple"
        | ColorTwo -> "green"
    )
    | Open_source -> (
        match Card.color card with
        | ColorZero -> "black"
        | ColorOne -> "blue"
        | ColorTwo -> "orange"
    )

let shape ?(plural=false) ~card = function
    | Classic -> (
        match Card.shape card with
        | ShapeZero ->
            if plural then "ovals" else "oval"
        | ShapeOne ->
            if plural then "diamonds" else "diamond"
        | ShapeTwo ->
            if plural then "squiggles" else "squiggle")
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
        primary = "#3f51b5";
        secondary = "#f44336";
        tertiary = "#899f51";
    }

let card_to_string ~theme card =
    let num_attr = num ~card theme in
    let fill_attr = fill ~card theme in
    let color_attr = color ~card theme in
    let plural = card.num != NumZero in
    let shape_attr = shape ~plural ~card theme in
    Printf.sprintf "%s %s %s %s" num_attr fill_attr color_attr shape_attr

let default_card_size = (1000.0, 1000.0)

let attr_entries attrs =
    let entries = List.fold_left (fun acc (key, value) ->
        if key != "" && value != "" then
            let entry = Printf.sprintf "%s='%s'" key value in
            entry :: acc
        else acc
    ) [] attrs in
    String.concat " " entries

let make_path ?(attrs=[]) ?(header="") data =
    let attrs_str = attr_entries attrs in
    Printf.sprintf "%s<path %s d='%s'/>" header attrs_str data

let make_group ?(attrs=[]) ?(header="") content =
    let attrs_str = attr_entries attrs in
    Printf.sprintf "%s<g %s>%s</g>" header attrs_str content

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
    let make_shape_svg card =
        let color = match Card.color card with
        | ColorZero -> "red"
        | ColorOne -> "purple"
        | ColorTwo -> "green"
        in
        let (defs, fill) = match Card.fill card with
        | FillZero -> (* open *) ("", "white")
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
            let fill = "url(#a)" in
            (defs, fill)
        | FillTwo -> (* solid *) ("", color)
        in
        let make_classic_path data =
            make_path ~attrs:[
                ("color", color);
                ("fill", fill);
                ("stroke", color);
                ("stroke-width", "10");
                ("stroke-linejoin", "round")
            ] ~header:defs data
        in
        match Card.shape card with
        | ShapeZero -> ( (* oval *)
            let data = match Card.num card with
            | NumZero -> Path_data.classic_oval_one
            | NumOne -> Path_data.classic_oval_two
            | NumTwo -> Path_data.classic_oval_three
            in
            make_classic_path data
        )
        | ShapeOne -> ( (* diamonds *)
            let data = match Card.num card with
            | NumZero -> Path_data.classic_diamond_one
            | NumOne -> Path_data.classic_diamond_two
            | NumTwo -> Path_data.classic_diamond_three
            in
            make_classic_path data
        )
        | ShapeTwo -> ( (* squiggle *)
            let data = match Card.num card with
            | NumZero -> Path_data.classic_squiggle_one
            | NumOne -> Path_data.classic_squiggle_two
            | NumTwo -> Path_data.classic_squiggle_three
            in
            make_classic_path data
        )

    let make_card_svgs ~width ~height card =
        let (vw, vh) = default_card_size in
        let content = make_shape_svg card in
        [make_svg ~width ~height ~vx:0.0 ~vy:0.0 ~vw ~vh content]
end

module Card_svg_open_source : CARD_SVG_THEME = struct
    let make_shape_svg card =
        let color = match Card.color card with
        | ColorZero -> "#000000" (* black *)
        | ColorOne -> "#336791" (* blue *)
        | ColorTwo -> "#ef760e" (* orange *)
        in
        let (defs, fill) = match Card.fill card with
        | FillZero -> (* open *) ("", "white")
        | FillOne -> (* shaded *)
            let defs = Printf.sprintf
                {eodefs|
                    <defs>
                        <pattern id="a" height="200" width="200" patternUnits="userSpaceOnUse">
                            <path fill="%s" d="%s"/>
                        </pattern>
                    </defs>
                |eodefs}
                color Path_data.pattern_circuit_board
            in
            let fill = "url(#a)" in
            (defs, fill)
        | FillTwo -> (* solid *) ("", color)
        in
        let make_open_source_group content =
            make_group ~attrs:[
                ("fill", fill);
                ("color", color);
                ("stroke", color);
                ("stroke-width", "5")
            ] ~header:defs (make_path content)
        in
        match Card.shape card with
        | ShapeZero -> ( (* penguin *)
            let content = match Card.num card with
            | NumZero -> Path_data.open_source_penguin_one
            | NumOne -> Path_data.open_source_penguin_two
            | NumTwo -> Path_data.open_source_penguin_three
            in
            make_open_source_group content
        )
        | ShapeOne -> ( (* elephant *)
            let content = match Card.num card with
            | NumZero -> Path_data.open_source_elephant_one
            | NumOne -> Path_data.open_source_elephant_two
            | NumTwo -> Path_data.open_source_elephant_three
            in
            make_open_source_group content
        )
        | ShapeTwo -> ( (* camel *)
            let content = match Card.num card with
            | NumZero -> Path_data.open_source_camel_one
            | NumOne -> Path_data.open_source_camel_two
            | NumTwo -> Path_data.open_source_camel_three
            in
            make_open_source_group content
        )

    let make_card_svgs ~width ~height card =
        let (vw, vh) = default_card_size in
        let content = make_shape_svg card in
        [make_svg ~width ~height ~vx:0.0 ~vy:0.0 ~vw ~vh content]
end

let make_card_svgs ~width ~height ~theme card =
    let f = match theme with
    | Classic -> Card_svg_classic.make_card_svgs
    | Open_source -> Card_svg_open_source.make_card_svgs
    in f ~width ~height card
