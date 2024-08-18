let build_mat l =
  let pos = ref [] in
  let h, w, z = (List.length l, String.length (List.hd l), ref (0, 0)) in
  let build x y =
    let case = (List.nth l y).[x] in
    if case = '0' then z := (x, y);
    if case >= '0' && case <= '9' then pos := (x, y) :: !pos;
    if case = '#' then '#' else '.'
  in
  (!pos, !z, Array.init h (fun y -> Array.init w (fun x -> build x y)))

let dist mat b a =
  let conitnue Bfs.{ to_exp; _ } = List.mem b to_exp in
  Bfs.bfs Pos.dir4 [ '#' ] mat conitnue a

let all_dist mat pos =
  let dists = Hashtbl.create 1024 in
  let add x y d =
    Hashtbl.add dists (x, y) d;
    Hashtbl.add dists (y, x) d
  in
  let rec aux = function
    | [] -> dists
    | x :: xs ->
        List.iter (fun y -> add x y (dist mat x y)) xs;
        aux xs
  in
  aux pos

let dist_list d pos =
  let rec aux acc = function
    | [ _ ] | [] -> acc
    | a :: b :: tl -> aux (Hashtbl.find d (a, b) + acc) (b :: tl)
  in
  aux 0 pos

let min_dist d pos = List.min (List.map (dist_list d) pos)

let main is_p1 l =
  let pos, zero, mat = build_mat l in
  let perm = List.permutations pos in
  let hd0 x = List.hd x = zero in
  let perm =
    if is_p1 then perm
    else List.(filter hd0 perm |> map (fun x -> zero :: rev x))
  in
  min_dist (all_dist mat pos) perm |> string_of_int

let p1 = main true
let p2 = main false
