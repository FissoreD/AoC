let size = (4, 4)
let dest = (3, 3)

module M : Pos.M = struct
  let neigh = Pos.neigh4
  let valid = Pos.valid_size size
end

module P = Pos.Make (M)

let give_dir (x1, y1) (x2, y2) =
  match (x1 - x2, y1 - y2) with
  | _, 1 -> (0, "U")
  | _, -1 -> (1, "D")
  | 1, _ -> (2, "L")
  | _, _ -> (3, "R")

let is_open s x = s.[x] > 'a' && s.[x] <= 'f'

let give_neigh give_neigh (pos, pref) =
  if pos = dest then []
  else
    let hash = Digest.string pref |> Digest.to_hex in
    let neigh = give_neigh pos in
    let dirs = List.map (give_dir pos) neigh in
    let rec aux = function
      | [], _ | _, [] -> []
      | (x, s) :: xs, y :: ys when is_open hash x ->
          (y, pref ^ s) :: aux (xs, ys)
      | _ :: xs, _ :: ys -> aux (xs, ys)
    in
    aux (dirs, neigh)

let bfs while_cond is_p1 hash =
  let hash_len = String.length hash in
  let dist = ref 0 in
  let max_len = ref 0 in
  let pos = ref [ ((0, 0), hash) ] in
  while not (while_cond !pos) do
    if (not is_p1) && List.mem_assoc dest !pos then max_len := !dist;
    pos := List.map (give_neigh P.get_neigh) !pos |> List.flatten;
    incr dist
  done;
  if is_p1 then String.sub (List.assoc dest !pos) hash_len !dist
  else !max_len |> string_of_int

let p1 l = bfs (List.mem_assoc dest) true (List.hd l)
let p2 l = bfs (( = ) []) false (List.hd l)
