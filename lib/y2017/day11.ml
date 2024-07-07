let to_dir = function
  | "n" -> (0, 2)
  | "s" -> (0, -2)
  | "ne" -> (-1, 1)
  | "se" -> (-1, -1)
  | "nw" -> (1, 1)
  | _ -> (1, -1)

let dist (x, y) = (abs x + abs y) / 2
let neighs = [ (2, 0); (-2, 0); (-1, -1); (1, -1); (-1, -1); (1, 1) ]
let parse l = List.hd l |> String.split_on_char ',' |> List.map to_dir
let p1 l = List.fold_left Pos.add (0, 0) (parse l) |> dist |> string_of_int

let p2 l =
  let new_conf d p' = (max (dist p') d, p') in
  let l = parse l in
  List.fold_left (fun (d, p) dir -> new_conf d (Pos.add dir p)) (0, (0, 0)) l
  |> fst |> string_of_int
