let valid_digest ~prefix pref suff =
  Digest.(string (pref ^ string_of_int suff) |> to_hex) |> String.starts_with ~prefix
  

let rec find_digest ~prefix p n = 
  if valid_digest ~prefix p n then n else find_digest ~prefix p (n + 1)

let p1 s = 
  let s = List.hd s in
  print_int @@ find_digest ~prefix:"00000" s 0

let p2 s =
  let s = List.hd s in
  print_int @@ find_digest ~prefix:"000000" s 0

