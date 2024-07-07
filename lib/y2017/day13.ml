let parse l = List.map Utils.all_ints l |> List.map Tuple.to2
let get_pos i j = i mod ((j * 2) - 2)

let rec count_cought = function
  | [] -> 0
  | (i, d) :: tl -> (if get_pos i d == 0 then i * d else 0) + count_cought tl

let p1 l = count_cought (parse l) |> string_of_int

let rec safe_start l n =
  if List.for_all (fun (i, j) -> get_pos (i + n) j <> 0) l then n
  else safe_start l (n + 1)

let p2 l = safe_start (parse l) 0 |> string_of_int
