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
        | ColorOne -> "purple"
        | ColorTwo -> "green"

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
        | ColorOne -> "purple"
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
        let make_path path_d =
            Printf.sprintf
            {eoshape|
                %s
                <path
                    %s
                    color="%s"
                    stroke="%s"
                    stroke-width="10"
                    stroke-linejoin="round"
                    d="%s"
                />
            |eoshape}
            defs fill color color path_d
        in
        match Card.shape card with
        | ShapeZero -> ( (* oval *)
            let path_d = match Card.num card with
            | NumZero -> "M400 400a100 100 0 0 0-100 100 100 100 0 0 0 100 100h200a100 100 0 0 0 100-100 100 100 0 0 0-100-100H400z"
            | NumOne -> "M400 550a100 100 0 0 0-100 100 100 100 0 0 0 100 100h200a100 100 0 0 0 100-100 100 100 0 0 0-100-100H400zM400 250a100 100 0 0 0-100 100 100 100 0 0 0 100 100h200a100 100 0 0 0 100-100 100 100 0 0 0-100-100H400z"
            | NumTwo -> "M400 100a100 100 0 0 0-100 100 100 100 0 0 0 100 100h200a100 100 0 0 0 100-100 100 100 0 0 0-100-100H400zM400 400a100 100 0 0 0-100 100 100 100 0 0 0 100 100h200a100 100 0 0 0 100-100 100 100 0 0 0-100-100H400zM400 700a100 100 0 0 0-100 100 100 100 0 0 0 100 100h200a100 100 0 0 0 100-100 100 100 0 0 0-100-100H400z"
            in
            make_path path_d
        )
        | ShapeOne -> ( (* diamonds *)
            let path_d = match Card.num card with
            | NumZero -> "M501.18 400.838l-200 100 .89.443 199.11 99.557 200-100-200-100z"
            | NumOne -> "M501.2 249.41l-200 100 .89.443L501.2 449.41l200-100-200-100zM501.17 549.41l-200 99.998.89.446 199.11 99.556 200-100.002-200-99.998z"
            | NumTwo -> "M501.18 103.695l-200 100 .89.444 199.11 99.556 200-100-200-100zM501.18 400.838l-200 100 .89.443 199.11 99.557 200-100-200-100zM499.752 699.41l-200 99.998.889.446 199.11 99.556 200-100.002-199.999-99.998z"
            in
            make_path path_d
        )
        | ShapeTwo -> ( (* squiggle *)
            let path_d = match Card.num card with
            | NumZero -> "M652.023 409.844c-1.59-.032-3.211.06-4.861.283-12.65 2.777-24.645 14.29-36.2 20.732-22.248 12.41-47.617 20.576-73.377 20.287-19.86-.232-33.225-7.82-50.875-14.405l-30.33-9.943-9.783-1.918-8.805-2.353-12.719-1.552c-5.46-.839-5.293-1.572-11.74-1.65-21.358-.24-44.653 2.306-63.594 12.864-15.957 8.89-32.296 27.355-39.497 44.027l-4.274 12.535-2.897 7.714c-1.487 5.226-3.219 18.59-3.229 24.106-.03 19.575-.195 31.02 5.988 50.141 3.806 11.764 10.204 22.69 22.385 27.588 3.14 1.263 9.167 1.59 12.719 1.813 17.072 1.06 36.551-15.92 50.875-24.31 3.443-2.005 14.078-7.482 17.61-8.417l6.849-1.283 7.827-2.45 8.805-.944 6.848-1.35c10.665-1.514 10.684.087 19.568 1.35 3.346.472 5.302.078 8.805 1.052l15.654 6.18c13.032 4.83 26.465 9.113 40.113 11.678 6.32 1.186 5.948 2.496 13.697 2.593l26.416.79 7.827-.924h9.783l20.546-5.873c17.542-5.265 38.549-15.08 51.57-28.02l10.067-12.257c6.174-7.676 11.575-15.255 15.929-24.106l8.023-22.178c1.144-4.773 1.232-9.614 1.574-14.464 1.116-15.688 2.22-26.333-3.953-41.462-5.648-13.842-17.98-25.57-33.344-25.874z"
            | NumOne -> "M652.023 259.844c-1.59-.032-3.211.06-4.861.283-12.65 2.777-24.645 14.29-36.2 20.732-22.248 12.41-47.617 20.576-73.377 20.287-19.86-.232-33.225-7.82-50.875-14.405l-30.33-9.943-9.783-1.918-8.805-2.353-12.719-1.552c-5.46-.839-5.293-1.572-11.74-1.65-21.358-.24-44.653 2.306-63.594 12.864-15.957 8.89-32.296 27.355-39.497 44.027l-4.274 12.535-2.897 7.714c-1.487 5.226-3.219 18.59-3.229 24.106-.03 19.575-.195 31.02 5.988 50.141 3.806 11.764 10.204 22.69 22.385 27.588 3.14 1.263 9.167 1.59 12.719 1.813 17.072 1.06 36.551-15.92 50.875-24.31 3.443-2.005 14.078-7.482 17.61-8.417l6.849-1.283 7.827-2.45 8.805-.944 6.848-1.35c10.665-1.514 10.684.087 19.568 1.35 3.346.472 5.302.078 8.805 1.052l15.654 6.18c13.032 4.83 26.465 9.113 40.113 11.678 6.32 1.186 5.948 2.496 13.697 2.593l26.416.79 7.827-.924h9.783l20.546-5.873c17.542-5.265 38.549-15.08 51.57-28.02l10.067-12.257c6.174-7.676 11.575-15.255 15.929-24.106l8.023-22.178c1.144-4.773 1.232-9.614 1.574-14.464 1.116-15.688 2.22-26.333-3.953-41.462-5.648-13.842-17.98-25.57-33.344-25.874zM652.023 559.844a31.56 31.56 0 0 0-4.861.283c-12.65 2.777-24.645 14.29-36.2 20.732-22.248 12.41-47.617 20.576-73.377 20.287-19.86-.232-33.225-7.82-50.875-14.405l-30.33-9.943-9.783-1.918-8.805-2.353-12.719-1.552c-5.46-.839-5.293-1.572-11.74-1.65-21.358-.24-44.653 2.306-63.594 12.864-15.957 8.89-32.296 27.355-39.497 44.027l-4.274 12.535-2.897 7.714c-1.487 5.226-3.219 18.59-3.229 24.106-.03 19.575-.195 31.02 5.988 50.141 3.806 11.764 10.204 22.69 22.385 27.588 3.14 1.263 9.167 1.591 12.719 1.813 17.072 1.06 36.551-15.92 50.875-24.31 3.443-2.005 14.078-7.482 17.61-8.417l6.849-1.283 7.827-2.45 8.805-.944 6.848-1.35c10.665-1.514 10.684.087 19.568 1.35 3.346.472 5.303.078 8.805 1.052l15.654 6.18c13.032 4.83 26.465 9.113 40.113 11.678 6.32 1.186 5.948 2.496 13.697 2.593l26.416.79 7.827-.924h9.783l20.546-5.873c17.542-5.265 38.549-15.08 51.57-28.02l10.067-12.257c6.174-7.676 11.575-15.255 15.93-24.106l8.022-22.178c1.144-4.773 1.232-9.614 1.574-14.464 1.116-15.688 2.22-26.333-3.953-41.462-5.648-13.842-17.98-25.57-33.344-25.874z"
            | NumTwo -> "M652.023 109.844a31.56 31.56 0 0 0-4.861.283c-12.65 2.777-24.645 14.29-36.2 20.732-22.248 12.41-47.617 20.576-73.377 20.287-19.86-.232-33.225-7.82-50.875-14.405l-30.33-9.943-9.783-1.918-8.805-2.353-12.719-1.552c-5.46-.839-5.293-1.572-11.74-1.65-21.358-.24-44.653 2.306-63.594 12.864-15.957 8.89-32.296 27.355-39.497 44.027l-4.274 12.535-2.897 7.714c-1.487 5.226-3.219 18.59-3.229 24.106-.03 19.575-.195 31.02 5.988 50.141 3.806 11.764 10.204 22.69 22.385 27.588 3.14 1.263 9.167 1.591 12.719 1.813 17.072 1.06 36.551-15.92 50.875-24.31 3.443-2.005 14.078-7.482 17.61-8.417l6.849-1.283 7.827-2.45 8.805-.944 6.848-1.35c10.665-1.514 10.684.087 19.568 1.35 3.346.472 5.303.078 8.805 1.052l15.654 6.18c13.032 4.83 26.465 9.113 40.113 11.678 6.32 1.186 5.948 2.496 13.697 2.593l26.416.79 7.827-.924h9.783l20.546-5.873c17.542-5.265 38.549-15.08 51.57-28.02l10.067-12.257c6.174-7.676 11.575-15.255 15.93-24.106l8.022-22.178c1.144-4.773 1.232-9.614 1.574-14.464 1.116-15.688 2.22-26.333-3.953-41.462-5.648-13.842-17.98-25.57-33.344-25.874zM652.023 409.844c-1.59-.032-3.211.06-4.861.283-12.65 2.777-24.645 14.29-36.2 20.732-22.248 12.41-47.617 20.576-73.377 20.287-19.86-.232-33.225-7.82-50.875-14.405l-30.33-9.943-9.783-1.918-8.805-2.353-12.719-1.552c-5.46-.839-5.293-1.572-11.74-1.65-21.358-.24-44.653 2.306-63.594 12.864-15.957 8.89-32.296 27.355-39.497 44.027l-4.274 12.535-2.897 7.714c-1.487 5.226-3.219 18.59-3.229 24.106-.03 19.575-.195 31.02 5.988 50.141 3.806 11.764 10.204 22.69 22.385 27.588 3.14 1.263 9.167 1.59 12.719 1.813 17.072 1.06 36.551-15.92 50.875-24.31 3.443-2.005 14.078-7.482 17.61-8.417l6.849-1.283 7.827-2.45 8.805-.944 6.848-1.35c10.665-1.514 10.684.087 19.568 1.35 3.346.472 5.302.078 8.805 1.052l15.654 6.18c13.032 4.83 26.465 9.113 40.113 11.678 6.32 1.186 5.948 2.496 13.697 2.593l26.416.79 7.827-.924h9.783l20.546-5.873c17.542-5.265 38.549-15.08 51.57-28.02l10.067-12.257c6.174-7.676 11.575-15.255 15.929-24.106l8.023-22.178c1.144-4.773 1.232-9.614 1.574-14.464 1.116-15.688 2.22-26.333-3.953-41.462-5.648-13.842-17.98-25.57-33.344-25.874zM652.023 709.844a31.56 31.56 0 0 0-4.861.283c-12.65 2.777-24.645 14.29-36.2 20.732-22.248 12.41-47.617 20.576-73.377 20.287-19.86-.232-33.225-7.82-50.875-14.405l-30.33-9.943-9.783-1.918-8.805-2.353-12.719-1.552c-5.46-.839-5.293-1.572-11.74-1.65-21.358-.24-44.653 2.306-63.594 12.864-15.957 8.89-32.296 27.355-39.497 44.027l-4.274 12.535-2.897 7.714c-1.487 5.226-3.219 18.59-3.229 24.106-.03 19.575-.195 31.02 5.988 50.141 3.806 11.764 10.204 22.69 22.385 27.588 3.14 1.263 9.167 1.591 12.719 1.813 17.072 1.06 36.551-15.92 50.875-24.31 3.443-2.005 14.078-7.482 17.61-8.417l6.849-1.283 7.827-2.45 8.805-.944 6.848-1.35c10.665-1.514 10.684.087 19.568 1.35 3.346.472 5.303.078 8.805 1.052l15.654 6.18c13.032 4.83 26.465 9.113 40.113 11.678 6.32 1.186 5.948 2.496 13.697 2.593l26.416.79 7.827-.924h9.783l20.546-5.873c17.542-5.265 38.549-15.08 51.57-28.02l10.067-12.257c6.174-7.676 11.575-15.255 15.93-24.106l8.022-22.178c1.144-4.773 1.232-9.614 1.574-14.464 1.116-15.688 2.22-26.333-3.953-41.462-5.648-13.842-17.98-25.57-33.344-25.874z"
            in
            make_path path_d
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
