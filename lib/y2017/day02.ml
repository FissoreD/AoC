let parse l = List.map Utils.all_ints l

let p1 l =
  let l = parse l in
  List.(sum (map max l) - sum (map min l)) |> string_of_int

let find_pair x = List.find (fun y -> x mod y = 0)

let rec find_pair_l p l =
  let x, xs = (List.hd l, List.tl l) in
  try x / find_pair x (p @ xs) with Not_found -> find_pair_l (x :: p) xs

let p2 l = List.(sum @@ map (find_pair_l []) (parse l)) |> string_of_int
