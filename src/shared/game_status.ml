type t =
    | New
    | Started
    | Complete

let to_string = function
    | New -> "new"
    | Started -> "started"
    | Complete -> "complete"

let of_string = function
    | "new" -> New
    | "started" -> Started
    | "complete" -> Complete
    | ts -> raise (Invalid_argument ("Unknown game status: " ^ ts))
