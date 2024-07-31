let parse l = List.hd l |> String.to_list |> List.map int_of_char
let diff = int_of_char 'a' - int_of_char 'A' |> abs
let react a b = abs (a - b) = diff

let rec react_list = function
  | [] -> []
  | [ a ] -> [ a ]
  | x1 :: x2 :: xs when react x1 x2 -> react_list xs
  | x :: xs -> x :: react_list xs

let rec full_react l =
  let l' = react_list l in
  if l = l' then l else full_react l'

let full_react_len l = List.length (full_react l)
let p1 l = full_react_len (parse l) |> string_of_int

let remove n l =
  List.filter (fun e -> e <> n && e <> n + diff && e <> n - diff) l

let p2 l =
  let l = parse l |> full_react in
  let start, stop = (int_of_char 'a', int_of_char 'z' + 1) in
  let rec aux i =
    if i = stop then max_int
    else min (full_react_len (remove i l)) (aux (i + 1))
  in
  aux start |> string_of_int
