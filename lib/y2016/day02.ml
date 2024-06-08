let parse_line = List.map (Str.(split (regexp "")))

let move p = function
  | "U" -> if p < 4 then p else p - 3
  | "D" -> if p > 6 then p else p + 3
  | "L" -> if p mod 3 = 1 then p else p - 1
  | _   -> if p mod 3 = 0 then p else p + 1

let rec do_lines to_str ~move p = function
  | [] -> ""
  | hd :: tl ->
    let pos = List.fold_left move p hd in
    (to_str pos) ^ do_lines to_str ~move pos tl

let p1 l = do_lines string_of_int ~move 5 (parse_line l)

let move p = function
  | "R" -> if (p >= 2 && p < 4) || (p >= 5 && p < 9) || (p >= 10 && p < 12) then p + 1 else p
  | "L" -> if (p > 2 && p <= 4) || (p > 5 && p <= 9) || (p > 10 && p <= 12) then p - 1 else p
  | "U" ->
      if (p > 5 && p < 9) || (p > 9 && p < 13) then p - 4 else
      if p = 13 || p = 3 then p - 2 else p
  | _ ->
      if (p > 5 && p < 9) || (p > 1 && p < 5) then p + 4 else
      if p = 1 || p = 11 then p + 2 else p

let p2 l = do_lines (Printf.sprintf "%X") ~move 5 (parse_line l)
