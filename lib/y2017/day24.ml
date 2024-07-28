let get graph k = Hashtbl.find_opt graph k |> Option.value ~default:[]

let parse l =
  let parse l = List.sort compare (Utils.all_ints l) |> Tuple.to2 in
  let l = List.map parse l in
  let graph = Hashtbl.create 1024 in
  let add (k, v) = Hashtbl.replace graph k (v :: get graph k |> List.no_dup) in
  List.iter add l;
  List.iter (fun (x, y) -> add (y, x)) l;
  graph

let hash a b = if a <= b then (a * 1000) + b else (b * 1000) + a

let rec continue maxf graph node used acc node1 =
  let hash = hash node node1 in
  if List.mem hash used then acc
  else find_max maxf graph node1 (hash :: used) (Pos.add acc (1, node1 + node))

and find_max maxf graph node used acc =
  List.fold_left maxf acc
  @@ List.map (continue maxf graph node used acc) (get graph node)

let p1 l =
  let max x y = if snd x > snd y then x else y in
  find_max max (parse l) 0 [] (0, 0) |> snd |> string_of_int

let p2 l = find_max max (parse l) 0 [] (0, 0) |> snd |> string_of_int
