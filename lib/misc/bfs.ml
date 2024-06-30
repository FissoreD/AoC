type r = { dist : int; to_exp : Pos.t list }

module type M = sig
  type t

  val mat : t array array
  val obstacle : t list
  val neigh : Pos.t list
  val goal : r -> bool
end

module Make (X : M) = struct
  let explored = ref []
  let size = (Array.length X.mat.(0), Array.length X.mat)

  module M : Pos.M = struct
    let neigh = X.neigh

    let valid x =
      let res =
        Pos.valid_size size x
        && (not (List.mem X.mat.(snd x).(fst x) X.obstacle))
        && not (List.mem x !explored)
      in
      explored := x :: !explored;
      res
  end

  module Pos = Pos.Make (M)

  let bfs s =
    let dist = ref 0 in
    let to_exp = ref [ s ] in
    while X.goal { dist = !dist; to_exp = !to_exp } do
      let new_expl = List.map Pos.get_neigh !to_exp |> List.flatten in
      to_exp := List.no_dup new_expl;
      incr dist
    done;
    !dist
end
