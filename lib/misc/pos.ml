let neigh4 = [ (1, 0); (-1, 0); (0, 1); (0, -1) ]

type neigh4 = U | R | D | L

let get mat (x, y) = mat.(y).(x)
let dir2pos = function U -> (0, -1) | D -> (0, 1) | L -> (-1, 0) | R -> (1, 0)
let neigh8 = neigh4 @ [ (1, 1); (-1, 1); (1, -1); (-1, -1) ]
let valid00 (x, y) = x >= 0 && y >= 0
let valid_size (w, h) ((x, y) as p) = valid00 p && x < w && y < h
let rot_right = function U -> R | R -> D | D -> L | L -> U
let rot_left = function U -> L | L -> D | D -> R | R -> U
let rot_inv = function U -> D | L -> R | R -> L | D -> U

let valid_mat m p =
  let w, h = (List.length (List.hd m), List.length m) in
  valid_size (w, h) p

type t = int * int [@@deriving show]

module type M = sig
  val neigh : t list
  val valid : t -> bool
end

let add (x, y) (x', y') = (x + x', y + y')
let add_dir d = add (dir2pos d)

module Make (M : M) = struct
  let get_neigh p = List.map (add p) M.neigh |> List.filter M.valid
  let valid = M.valid
end

let compare (x, y) (x', y') =
  match compare x x' with 0 -> compare y y' | n -> n
