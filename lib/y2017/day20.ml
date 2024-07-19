let parse l =
  match Utils.all_ints l with
  | [ a; b; c; d; e; f; g; h; i ] -> ((a, d, g), (b, e, h), (c, f, i))
  | _ -> Utils.error 2017 20 l

let trd (_, _, a) = a
let dist_sq (a, b, c) = (a * a) + (b * b) + (c * c)

let p1 l =
  (* Returns the row with lowest acceleration *)
  let acc_vec (a, b, c) = (trd a, trd b, trd c) in
  let l = List.mapi (fun i e -> (i, dist_sq (acc_vec (parse e)))) l in
  let min_dist (ai, ad) (i, d) = if ad <= d then (ai, ad) else (i, d) in
  List.fold_left min_dist (-1, max_int) l |> fst |> string_of_int

(*
  formula for in 1d plan
  pos   speed  acc
    1     2     3
 1  6     5     3     2 + 3
 2 14     8     3     2 + 3 * 2
 3 25    11     3     2 + 3 * 3

 f n =  p + s * n + a * (n * (n + 1)) / 2
 f 3 =  1 + 2 * 3 + 3 * (3 * (3 + 1)) / 2 = 25
 
 Two curves meet by posing f1(n) = f2(n) that is
 p0 + (v0+a0/2)*n + a0/2*n² = p1 + (v1+a1/2)*n + a1/2*n²
*)

(* get position of a curve in the 3d plan wrt a step nb *)
let get_pos n ((x, sx, ax), (y, sy, ay), (z, sz, az)) =
  let f p s a = p + (s * n) + (a * (n * (n + 1)) / 2) in
  (f x sx ax, f y sy ay, f z sz az)

let inters (p0, s0, a0) (p1, s1, a1) =
  let round x = Float.round x |> int_of_float in
  let p0, s0, a0 = (float p0, float s0, float a0) in
  let p1, s1, a1 = (float p1, float s1, float a1) in
  let c, b, a = (p0 -. p1, s0 -. s1 +. ((a0 -. a1) /. 2.), (a0 -. a1) /. 2.) in
  List.map round (Utils.parabola_zeros (a, b, c))

(* check if two curves ever intersect *)
let inters r1 r2 =
  List.filter (fun x -> get_pos x r1 = get_pos x r2) (inters (trd r1) (trd r2))

(* get all intersection points *)
let rec all_meet acc = function
  | [] -> List.flatten acc |> List.no_dup |> List.sort compare
  | h :: l -> all_meet ((List.map (inters h) l |> List.flatten) :: acc) l

(* remove all colliding curves *)
let rec remove curves = function
  | [] -> curves
  | h :: l ->
      let pos = List.map (get_pos h) curves in
      remove List.(filter (fun e -> count (get_pos h e) pos = 1) curves) l

let p2 l =
  let l = List.map parse l in
  List.length (remove l (all_meet [] l)) |> string_of_int
