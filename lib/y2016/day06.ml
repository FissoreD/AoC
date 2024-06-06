open Array

let build_column mat nb = init (length mat) (fun i -> mat.(i).(nb))
let rotate_mat mat = init (length mat.(0)) (build_column mat)

let find_max ~f l =
  let module SM = Map.Make (Char) in
  let m = ref SM.empty in
  let add k = try incr (SM.find k !m) with _ -> m := SM.add k (ref 1) !m in
  let cond k v (k1, v1) = if k1 = ' ' || f !v v1 then (k, !v) else (k1, v1) in
  iter add l;
  SM.fold cond !m (' ', 0) |> fst

let main f l =
  let l = List.map Str.(split (regexp "")) l in
  let l = List.map (List.map (Fun.flip String.get 0)) l in
  let l = of_list (List.map of_list l) in
  let rot = rotate_mat l in
  String.init (length rot) (fun i -> find_max ~f rot.(i)) |> print_string

let p1 = main ( > )
let p2 = main ( < )
