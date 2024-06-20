let p1 l =
  let str_length l =
    let l = Str.global_replace (Str.regexp {|\\[\"\\]|}) "." l in
    let rex_hex = Str.regexp {|\\x[0-9a-fA-F][0-9a-fA-F]|} in
    let l = Str.global_replace rex_hex "." l in
    String.length l - 2
  in
  let char_tot = List.sum (List.map String.length l) in
  let char_mem = List.sum (List.map str_length l) in
  char_tot - char_mem |> string_of_int

let p2 l =
  let str_length l =
    let l = Str.global_replace (Str.regexp {|[\\\"]|}) {|..|} l in
    String.length l + 2
  in
  let char_tot = List.sum (List.map str_length l) in
  let char_mem = List.sum (List.map String.length l) in
  char_tot - char_mem |> string_of_int
