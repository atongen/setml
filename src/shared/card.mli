type attr =
  | AttrZero
  | AttrOne
  | AttrTwo

type t = {
  num: attr;
  fill: attr;
  color: attr;
  shape: attr;
}

val num : t -> attr
val fill : t -> attr
val color : t -> attr
val shape : t -> attr

val attr_to_int : attr -> int
val attr_of_int : int -> attr

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

module Infix : sig
  val (--) : int -> int -> t list
  val (--^) : int -> int -> t list
end

val deck : unit -> t array

val is_set : t -> t -> t -> bool

val complete : t -> t -> t

val find_sets : t list -> (t * t * t) list

val find_non_sets : t list -> (t * t * t) list

val count_sets : t list -> int

val count_non_sets : t list -> int

val exists_set : t list -> bool

val exists_non_set : t list -> bool

val next_set_and_indexes : t list -> ((int * t) * (int * t) * (int * t)) option

val next_set_and_indexes_of_opt_array : (t option) array -> ((int * t) * (int * t) * (int * t)) option

val make_board : int -> t list
