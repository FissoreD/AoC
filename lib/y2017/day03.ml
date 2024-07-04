let rec corner_square x n = if x * x >= n then x else corner_square (x + 2) n

let dist n =
  let cs = corner_square 1 n in
  let xx = n + (cs * 2) - 2 in
  let sq = cs * cs in
  let n = if xx > sq then n else xx in
  let diff = sq - n in
  let crown_dist = cs / 2 in
  let center_dist = diff - crown_dist + if diff >= cs then -cs + 1 else 0 in
  crown_dist + abs center_dist

let p1 l = dist (int_of_string (List.hd l)) |> string_of_int
let turn_left = function Pos.U -> Pos.L | L -> D | D -> R | R -> U

let p2 l =
  let goal = int_of_string (List.hd l) in
  let tbl = Hashtbl.create 1024 in
  let get p = Hashtbl.find_opt tbl p |> Option.value ~default:0 in
  let get_val p = List.(sum (map (fun x -> get (Pos.add x p)) Pos.neigh8)) in
  let to_turn (x, y) =
    (abs x = abs y && (x + y <> 0 || y >= 0)) || (x + y = 1 && x > 0)
  in
  let rec aux dir pos =
    let pos = Pos.add_dir dir pos in
    let v = get_val pos in
    Hashtbl.add tbl pos v;
    if v > goal then v else aux (if to_turn pos then turn_left dir else dir) pos
  in
  Hashtbl.add tbl (0, 0) 1;
  aux R (0, 0) |> string_of_int
