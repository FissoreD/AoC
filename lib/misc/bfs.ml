type r = { dist : int; to_exp : Pos.t list }

let bfs neigh obstacle grid continue start =
  let exception AllExplored in
  let mat_size = (Array.length grid.(0), Array.length grid) in
  let is_free p = not (List.mem (Pos.get grid p) obstacle) in
  let is_free p = Pos.valid_size mat_size p && is_free p in
  let dist = ref 0 in
  let explored = Hashtbl.create 1024 in
  let valid x =
    let res = is_free x && not (Hashtbl.mem explored x) in
    Hashtbl.add explored x 0;
    res
  in
  let get_neigh p = List.map (Pos.add p) neigh |> List.filter valid in
  let to_exp = ref [ start ] in
  to_exp := List.filter is_free !to_exp;
  try
    while not (continue { dist = !dist; to_exp = !to_exp }) do
      let new_expl = List.map get_neigh !to_exp |> List.flatten in
      to_exp := List.no_dup new_expl;
      if !to_exp = [] then raise AllExplored;
      incr dist
    done;
    !dist
  with AllExplored -> max_int
