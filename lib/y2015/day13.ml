module HappyMap = Map.Make (String)

let parse_row map r =
  let r = String.split_on_char ' ' r in
  let fst = List.hd r in
  let lst =
    let lst = List.hd (List.rev r) in
    String.sub lst 0 (String.length lst - 1)
  in
  let int = int_of_string (List.nth r 3) in
  let coef = if List.mem "gain" r then 1 else ~-1 in
  (fst, HappyMap.add (fst ^ lst) (int * coef) map)

let parse_rows l =
  let rec aux map seen = function
    | [] -> (map, seen)
    | hd :: tl ->
        let fst, map = parse_row map hd in
        let seen = if List.mem fst seen then seen else fst :: seen in
        aux map seen tl
  in
  aux HappyMap.empty [] l

let rec rotate_list l = function
  | 0 -> l
  | n -> (
      match l with
      | [] -> Utils.error 2015 13 "n should be smaller then len(l)"
      | x :: xs -> rotate_list (xs @ [ x ]) (n - 1))

let all_rotation l =
  let rec aux = function 0 -> [] | n -> rotate_list l n :: aux (n - 1) in
  aux (List.length l)

let get_happy_lvl map a b =
  HappyMap.find (a ^ b) map + HappyMap.find (b ^ a) map

let rec find_happiness_level map fst = function
  | [] -> invalid_arg "y15/d13 -> invalid case (1)"
  | [ hd ] -> get_happy_lvl map fst hd
  | hd :: tl -> find_happiness_level_rot map fst hd tl

and find_happiness_level_rot map fst h1 tl =
  let rot = all_rotation tl in
  List.map
    (function
      | [] -> invalid_arg "y15/d13 -> invalid case (2)"
      | h2 :: _ as tl ->
          get_happy_lvl map h1 h2 + find_happiness_level map fst tl)
    rot
  |> List.fold_left max min_int

let find_happiness_level map = function
  | [] | [ _ ] -> 0
  | [ h1; h2 ] -> get_happy_lvl map h1 h2
  | fst :: tl -> find_happiness_level_rot map fst fst tl

let p1 l =
  let map, names = parse_rows l in
  find_happiness_level map names |> string_of_int

let p2 l =
  let map, names = parse_rows l in
  let names = "_" :: names in
  let add_map m a = HappyMap.(add ("_" ^ a) 0 m |> add (a ^ "_") 0) in
  let map = List.fold_left add_map map names in
  find_happiness_level map names |> string_of_int
