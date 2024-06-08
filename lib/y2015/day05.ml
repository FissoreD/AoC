let appears regexp s =
  try Str.search_forward regexp s 0 |> ignore; true 
  with Not_found -> false

let p1 s = 
  let is_good_string s =
    let vowels = "[aeiou]" in
    let vowels = Str.regexp (vowels ^ ".*" ^ vowels ^ ".*" ^ vowels) in
    let consec = Str.regexp "\\(.\\)\\1" in
    let exclus = Str.regexp "\\(ab\\)\\|\\(cd\\)\\|\\(pq\\)\\|\\(xy\\)" in
    appears vowels s && 
      appears consec s && 
      not (appears exclus s) in
  List.filter is_good_string s |> List.length |> string_of_int

let p2 s = 
  let is_good_string s =
    let repeat = Str.regexp "\\(.\\).\\1" in
    let repeat_sep = Str.regexp "\\(..\\).*\\1" in
    appears repeat s && appears repeat_sep s in
  List.filter is_good_string s |> List.length |> string_of_int
