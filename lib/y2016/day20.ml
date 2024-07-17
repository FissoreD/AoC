let l2c = function [ a; b ] -> (a, b) | _ -> Utils.error 2016 20 ""

let parse l =
  List.map (Utils.all_ints ~only_pos:true) l
  |> List.map l2c |> List.sort compare

let rec find_fin_free min = function
  | [] -> Utils.error 2016 20 "No min is present"
  | (x, _) :: _ when x > min -> min
  | (_, y) :: tl -> find_fin_free (max min (y + 1)) tl

let p1 l = parse l |> find_fin_free 0 |> string_of_int

let rec allowed_ip min = function
  | [] -> 0
  | (x, y) :: tl ->
      let add_free = if x > min then 1 else 0 in
      add_free + allowed_ip (max (min + add_free) (y + 1)) tl

let p2 l = parse l |> allowed_ip 0 |> string_of_int
