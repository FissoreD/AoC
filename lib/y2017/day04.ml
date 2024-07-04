let rec valid = function
  | [] -> true
  | x :: xs -> (not (List.mem x xs)) && valid xs

let parse = String.split_on_char ' '
let p1 l = List.(filter valid (map parse l) |> length) |> string_of_int

let p2 l =
  let open List in
  let sort_str x = String.to_list x |> sort Char.compare in
  let split_sort x = parse x |> map sort_str in
  filter valid (map split_sort l) |> length |> string_of_int
