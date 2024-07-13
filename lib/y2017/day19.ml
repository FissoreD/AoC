open Pos

let build_path l = Array.map String.to_array (Array.of_list l)

let rec turn_choice l pos d1 d2 n =
  let d = if get l (add_dir d1 pos) = ' ' then d2 else d1 in
  explore l (add_dir d pos) d (n + 1)

and explore l pos dir n lett =
  match get l pos with
  | ' ' -> (n, List.rev lett)
  | '+' -> (
      match dir with
      | D | U -> turn_choice l pos L R n lett
      | L | R -> turn_choice l pos U D n lett)
  | x when x >= 'A' && x <= 'Z' ->
      explore l (add_dir dir pos) dir (n + 1) (x :: lett)
  | _ -> explore l (add_dir dir pos) dir (n + 1) lett

let main l =
  let l = build_path l in
  explore l (Array.index_of l.(0) '|', 0) D 0 []

let p1 l = main l |> snd |> String.of_list
let p2 l = main l |> fst |> string_of_int
