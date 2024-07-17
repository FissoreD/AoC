let parse l = List.(map Utils.all_ints (tl (tl l)) |> map Tuple.to6)
let used (_, _, _, u, _, _) = u
let avail (_, _, _, _, a, _) = a
let pos (x, y, _, _, _, _) = (x, y)
let is_viable_pair a b = pos a <> pos b && used a <> 0 && used a < avail b

let get_viable_pairs l =
  let vp_nb e = List.length (List.filter (is_viable_pair e) l) in
  let rec aux = function [] -> 0 | hd :: tl -> vp_nb hd + aux tl in
  aux l

let p1 l = get_viable_pairs (parse l) |> string_of_int

let build_mat lab =
  let ai = Array.init in
  let poss = List.map pos lab in
  let w = List.max (List.map fst poss) + 1 in
  let h = List.max (List.map snd poss) + 1 in
  let rec find_empty = function
    | [] -> Utils.error 2016 22 "empty list"
    | a :: _ when used a = 0 -> pos a
    | _ :: l -> find_empty l
  in
  let find p l = List.find (fun x -> pos x = p) l in
  let ch p = if used p > 400 then '#' else '.' in
  (w - 1, find_empty lab, ai h (fun y -> ai w (fun x -> ch (find (x, y) lab))))

(*
  From my input problem, the solution is given by:
    shortest path from empty cell to start +
    shortest path from (start to dest - 2) * 5 since we need to move the
    target, we need to relocate 5 times to empty space
*)
let p2 l =
  let w, empty, m = build_mat (parse l) in
  let module M : Bfs.M = struct
    type t = char

    let mat = m
    let obstacle = [ '#' ]
    let neigh = Pos.neigh4
    let goal Bfs.{ to_exp; _ } = not (List.mem (w, 0) to_exp)
  end in
  let module Bfs = Bfs.Make (M) in
  Bfs.bfs empty + ((w - 1) * 5) |> string_of_int
