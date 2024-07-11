type 'a node
and 'a t

val pp_node :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a node -> unit
val show_node : (Format.formatter -> 'a -> unit) -> 'a node -> string

val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val show : (Format.formatter -> 'a -> unit) -> 'a t -> string

exception Empty_ddl

val create : unit -> 'a t
val empty_node : 'a -> 'a node
val singleton : 'a -> 'a t
val get_next : 'a t -> 'a t
val get_node : 'a t -> 'a node
val get_node_opt : 'a t -> 'a node option
val get_value : 'a t -> 'a
val add_next_node : 'a node -> 'a t -> 'a t
val add_next_value : 'a -> 'a t -> 'a t
val skip : 'a t -> int -> 'a t
val singleton_circ : 'a -> 'a t
val find : 'a t -> 'a -> 'a t
