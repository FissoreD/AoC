let neigh4 = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]
let neigh8 = neigh4 @ [ (1, 1); (-1, 1); (1, -1); (-1, -1) ]
let valid00 (x, y) = x >= 0 && y >= 0
let valid_size (w, h) ((x, y) as p) = valid00 p && x < w && y < h

let valid_mat m p =
  let w, h = (List.length (List.hd m), List.length m) in
  valid_size (w, h) p

type t = int * int

module type M = sig
  val neigh : t list
  val valid : t -> bool
end

module Make (M : M) = struct
  let add (x, y) (x', y') = (x + x', y + y')
  let get_neigh p = List.map (add p) M.neigh |> List.filter M.valid
  let valid = M.valid
end

module CmpPos : Set.OrderedType = struct
  type t = int * int

  let compare (x, y) (x', y') =
    match compare x x' with 0 -> compare y y' | n -> n
end

module SetPos = Set.Make (CmpPos)
