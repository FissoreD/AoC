let parse l = List.hd l |> int_of_string
let rec mod2 = function 1 -> 0 | n -> 1 + mod2 (n / 2)
let rec pow2 = function 0 -> 1 | n -> 2 * pow2 (n - 1)
let p1 l = 1 + (2 * (parse l - pow2 (mod2 (parse l)))) |> string_of_int

let find old_win n =
  if old_win < n / 2 then old_win + 1
  else if old_win + 1 > n then old_win - n
  else old_win + 2

let find n =
  let rec aux old_win x =
    if x = n then if old_win = 0 then x else old_win
    else aux (find old_win x) (x + 1)
  in
  aux 0 0

let p2 l = find (int_of_string (List.hd l)) |> string_of_int
