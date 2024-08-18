type t = int * int

val pp : Format.formatter -> t -> unit
val show : t -> string

type dir4 = U | R | D | L

exception NotADir of char

val dir4 : t list
val dir8 : t list
val get : 'a array array -> t -> 'a
val dir2pos : dir4 -> t
val ch2dir : char -> dir4
val valid00 : t -> bool
val valid_mat : 'a list list -> t -> bool
val valid_size : t -> t -> bool
val rot_right : dir4 -> dir4
val rot_left : dir4 -> dir4
val rot_inv : dir4 -> dir4
val add : t -> t -> t
val sub : t -> t -> t
val map : (int -> int) -> t -> t
val mul : int -> t -> t
val add_dir : dir4 -> t -> t

module type M = sig
  val neigh : t list
  val valid : t -> bool
end

module Make (_ : M) : sig
  val neigh : t list
  val valid : t -> bool
  val get_neight : int * int -> t list
end

val compare : 'a * 'b -> 'a * 'b -> int
val sort_list : t list -> t list
val dist : int * int -> int * int -> int
