open Str

let is_abba s : bool =
  let rex = regexp {|\(.\)\(.\)\2\1|} in
  let rec aux pos =
    try let p = search_forward rex s pos in s.[p] <> s.[p+1] || aux (pos+1)
    with _ -> false
  in aux 0

let split_str s =
  let s = global_replace (regexp "\\]") "[" s in
  let s = String.split_on_char '[' s in
  let rec aux ext (a,b) = function
  | [] -> a,b
  | x::xs -> aux (not ext) (if ext then (x::a, b) else (a,x::b)) xs in
  aux true ([],[]) s


let valid_string s =
  let l_ext, l_int = split_str s in
  not (List.exists is_abba l_int) && List.exists is_abba l_ext

let p1 l = List.filter valid_string l |> List.length |> string_of_int

let find_all_aba l =
  List.map (Utils.find_all_overlap (regexp {|\(.\).\1|})) l
    |> List.flatten |> List.filter (fun x -> x.[0] <> x.[1])

let valid_string l =
  let l_ext, l_in = split_str l in
  let x = find_all_aba l_ext in
  let y = find_all_aba l_in in
  List.exists (fun x -> List.exists (fun y -> x.[0] = y.[1] && x.[1] = y.[0]) y) x

let p2 l = List.filter valid_string l |> List.length |> string_of_int
