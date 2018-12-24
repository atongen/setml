type t = Classic | Open_source | Hero
type palette = { primary : string; secondary : string; tertiary : string; }
val to_string : t -> string
val to_human_string : t -> string
val of_string : string -> t
val of_string_opt : string -> t option
val palette : t -> palette
val card_to_string : theme:t -> Card.t -> string
val card_opt_to_string : theme:t -> Card.t option -> string
val make_card_svg : width:float -> height:float -> theme:t -> Card.t option -> string
