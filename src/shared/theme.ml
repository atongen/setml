type t =
    | Classic
    | Open_source
    | Hero

type palette = {
    primary: string;
    secondary: string;
    tertiary: string;
}

let to_string = function
    | Classic -> "classic"
    | Open_source -> "open_source"
    | Hero -> "hero"

let to_human_string = function
    | Classic -> "Classic"
    | Open_source -> "Open Source"
    | Hero -> "Hero"

let of_string = function
    | "classic" -> Classic
    | "open_source" -> Open_source
    | "hero" -> Hero
    | ts -> raise (Invalid_argument ("Unknown theme: " ^ ts))

let of_string_opt s = try Some (of_string s)
    with Invalid_argument _s -> None

let num ~card = function
    | Classic | Open_source | Hero ->
        match Card.num card with
        | NumZero -> "one"
        | NumOne -> "two"
        | NumTwo -> "three"

let fill ~card = function
    | Classic | Open_source -> (
        match Card.fill card with
        | FillZero -> "open"
        | FillOne -> "shaded"
        | FillTwo -> "solid"
    )
    | Hero ->
        match Card.fill card with
        | FillZero -> "0"
        | FillOne -> "1"
        | FillTwo -> "2"

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
    | Hero -> (
        match Card.color card with
        | ColorZero -> "red"
        | ColorOne -> "blue"
        | ColorTwo -> "green"
    )


let shape ?(plural=false) ~card theme =
    let p ~plural word = if plural then (Printf.sprintf "%ss" word) else word in
    match theme with
    | Classic -> (
        let w = match Card.shape card with
        | ShapeZero -> "oval"
        | ShapeOne -> "diamond"
        | ShapeTwo -> "squiggle" in
        p ~plural w
    )
    | Open_source -> (
        let w = match Card.shape card with
        | ShapeZero -> "penguin"
        | ShapeOne -> "elephant"
        | ShapeTwo -> "camel" in
        p ~plural w
    )
    | Hero -> (
        let w = match Card.shape card with
        | ShapeZero -> "triangle"
        | ShapeOne -> "circle"
        | ShapeTwo -> "square" in
        p ~plural w
    )

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
    | Hero -> {
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

let card_opt_to_string ~theme = function
    | Some card -> card_to_string ~theme card
    | None -> "[empty]"

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
    Printf.sprintf "%s<path %s d='%s'/>" header attrs_str (String.trim data)

let make_rect ?(attrs=[]) () =
    let attrs_str = attr_entries attrs in
    Printf.sprintf "<rect %s/>" attrs_str

let make_group ?(attrs=[]) ?(header="") children =
    let attrs_str = attr_entries attrs in
    let contents = List.map String.trim children in
    Printf.sprintf "%s<g %s>%s</g>" header attrs_str (String.concat "" contents)

let make_svg ~width ~height ~vx ~vy ~vw ~vh content =
    Printf.sprintf
        {eosvg|
            <svg xmlns='http://www.w3.org/2000/svg' width='%f' height='%f' viewBox='%f %f %f %f'>
                %s
            </svg>
        |eosvg}
    width height vx vy vw vh content

module type CARD_SVG_THEME = sig
    val make_card_svg : Card.t -> string
end

module Card_svg_classic : CARD_SVG_THEME = struct
    let make_card_svg card =
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
end

module Card_svg_open_source : CARD_SVG_THEME = struct
    let make_card_svg card =
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
            ] ~header:defs [(make_path content)]
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
end

module Card_svg_hero : CARD_SVG_THEME = struct
    let (width, height) = match default_card_size with
        | (x, y) -> (string_of_float x, string_of_float y)

    let make_card_svg card =
        let (forground_color, background_color) = match Card.color card with
        | ColorZero -> ("#ac929c", "#e5dbdf") (* red *)
        | ColorOne -> ("#9295ac", "#dbdce5") (* blue *)
        | ColorTwo -> ("#a1ac92", "#e1e5db") (* green *)
        in
        let make_pattern ~forground_color path_data =
            let defs = Printf.sprintf
                {eodefs|
                    <defs>
                        <pattern id="a" height="200" width="200" patternUnits="userSpaceOnUse">
                            <path fill="%s" d="%s"/>
                        </pattern>
                    </defs>
                |eodefs}
                forground_color path_data
            in
            let fill = "url(#a)" in
            (defs, fill)
        in
        let (pattern_defs, pattern_fill) = match Card.fill card with
        | FillZero -> make_pattern ~forground_color Path_data.pattern_circuit_board
        | FillOne -> make_pattern ~forground_color Path_data.pattern_circuit_board
        | FillTwo -> make_pattern ~forground_color Path_data.pattern_circuit_board
        in
        let make_hero_shape path_data =
            String.concat "" [
                pattern_defs;
                (make_rect ~attrs:[ (* solid fill background color *)
                    ("fill", background_color);
                    ("width", width);
                    ("height", height)
                ] ());
                (make_rect ~attrs:[ (* pattern fill forground color *)
                    ("width", width);
                    ("height", height);
                    ("fill", pattern_fill);
                    ("color", forground_color);
                ] ());
                (make_path ~attrs:[ (* shape *)
                    ("fill", forground_color);
                    ("fill-opacity", "0.7");
                    ("color", forground_color);
                    ("stroke", forground_color);
                    ("stroke-width", "5");
                ] path_data)
            ]
        in
        (match Card.shape card with
        | ShapeZero -> ( (* triangle *)
            match Card.num card with
            | NumZero -> Path_data.hero_triangle_one
            | NumOne -> Path_data.hero_triangle_two
            | NumTwo -> Path_data.hero_triangle_three
        )
        | ShapeOne -> ( (* circle *)
            match Card.num card with
            | NumZero -> Path_data.hero_circle_one
            | NumOne -> Path_data.hero_circle_two
            | NumTwo -> Path_data.hero_circle_three
        )
        | ShapeTwo -> ( (* square *)
            match Card.num card with
            | NumZero -> Path_data.hero_square_one
            | NumOne -> Path_data.hero_square_two
            | NumTwo -> Path_data.hero_square_three
        ))
        |> make_hero_shape
end

let make_card_svg ~width ~height ~theme maybeCard =
    let (vw, vh) = default_card_size in
    let content = match theme with
    | Classic -> (
        match maybeCard with
        | Some card -> Card_svg_classic.make_card_svg card
        | None -> Path_data.logo_svg
    )
    | Open_source -> (
        match maybeCard with
        | Some card -> Card_svg_open_source.make_card_svg card
        | None -> Path_data.logo_svg
    )
    | Hero -> (
        match maybeCard with
        | Some card -> Card_svg_hero.make_card_svg card
        | None -> Path_data.logo_svg
    )
    in
    make_svg ~width ~height ~vx:0.0 ~vy:0.0 ~vw ~vh content
