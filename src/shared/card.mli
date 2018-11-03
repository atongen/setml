type num = NumZero | NumOne | NumTwo
type fill = FillZero | FillOne | FillTwo
type color = ColorZero | ColorOne | ColorTwo
type shape = ShapeZero | ShapeOne | ShapeTwo
type t = { num : num; fill : fill; color : color; shape : shape; }

val num_to_int : num -> int
val fill_to_int : fill -> int
val color_to_int : color -> int
val shape_to_int : shape -> int

val int_to_num : int -> num
val int_to_fill : int -> fill
val int_to_color : int -> color
val int_to_shape : int -> shape

val num : t -> num
val fill : t -> fill
val color : t -> color
val shape : t -> shape

val make : int -> int -> int -> int -> t
val of_int : int -> t
val of_int_opt : int -> t option
val of_int_list : int list -> t option list
val to_int : t -> int
val to_int_opt : t option -> int
val to_int_list : t option list -> int list
val to_string : t -> string
val compare : t -> t -> int
val equal : t -> t -> bool
val range : int -> int -> t list
module Infix :
  sig
    val ( -- ) : int -> int -> t list
    val ( --^ ) : int -> int -> t list
  end
val ( -- ) : int -> int -> t list
val ( --^ ) : int -> int -> t list
val deck : unit -> t array

val is_set : t -> t -> t -> bool
val is_triple_set : t * t * t -> bool
val complete : t -> t -> t
val find_sets : t list -> (t * t * t) list
val count_sets : t list -> int
val exists_set : t list -> bool
