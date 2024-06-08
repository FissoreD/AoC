let parse_line l = Utils.all_ints l |> List.rev (* calories are put first *)

let get_value =
  List.(fold_left2 (fun a x y -> map2 (fun a x -> a + y * x) a x) [0;0;0;0;0])

let get_best l =
  let mul a = List.map (max 0) (List.tl a) |> List.fold_left ( * ) 1 in
  List.map mul l |> List.fold_left max min_int

let get_quantities l =
  let infos = List.map parse_line l in
  let combos = Utils.all_combo ~max_sum:100 ~nb_items:(List.length infos) in
  List.map (get_value infos) combos

let p1 l = get_quantities l |> get_best |> string_of_int

let p2 l =
  get_quantities l |> List.filter (fun e -> List.hd e = 500)
    |> get_best |> string_of_int
