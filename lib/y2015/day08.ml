let p1 l = 
  let str_length l =
    let l = Str.global_replace (Str.regexp {|\\[\"\\]|}) "." l in
    let l = Str.global_replace (Str.regexp {|\\x[0-9a-fA-F][0-9a-fA-F]|}) "." l in
    String.length l - 2 
  in
  let char_tot = List.fold_left (+) 0 (List.map String.length l) in
  let char_mem = List.fold_left (+) 0 (List.map str_length l) in
  char_tot - char_mem |> print_int

let p2 l = 
  let str_length l =
    let l = Str.global_replace (Str.regexp {|[\\\"]|}) {|..|} l in
    String.length l + 2 
  in
  let char_tot = List.fold_left (+) 0 (List.map str_length l) in
  let char_mem = List.fold_left (+) 0 (List.map String.length l) in
  char_tot - char_mem |> print_int

