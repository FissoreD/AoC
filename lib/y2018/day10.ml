let parse l =
  let to_pos (a, b, c, d) = ((a, b), (c, d)) in
  let parse l = to_pos (Tuple.to4 (Utils.all_ints l)) in
  List.map parse l

(* Heuristic to stop: forall point p, it exists a point p' at dist 1 or 2 *)
let stop l =
  let is12 d = d = 1 || d = 2 in
  let has_neigh l (x, _) = List.exists (fun (e, _) -> is12 (Pos.dist e x)) l in
  List.for_all (has_neigh l) l

let rec update n l =
  let l = List.map (fun (x, y) -> (Pos.add x y, y)) l in
  if stop l then (n, l) else update (n + 1) l

let print l =
  let l = List.map fst l in
  let minmax l = (List.min l, List.max l) in
  let (x1, x2), (y1, y2) = (minmax (List.map fst l), minmax (List.map snd l)) in
  let res = ref "\n" in
  let add s = res := !res ^ s in
  for y = y1 to y2 do
    for x = x1 to x2 do
      add (if List.mem (x, y) l then "#" else ".")
    done;
    if y <> y2 then add "\n"
  done;
  !res

let p1 l = update 0 (parse l) |> snd |> print
let p2 l = update 0 (parse l) |> fst |> string_of_int
