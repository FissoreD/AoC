include List

let sum = List.fold_left ( + ) 0
let mul = List.fold_left ( * ) 1
let min = List.fold_left min max_int
let max = List.fold_left max min_int
let cat = List.fold_left ( ^ ) ""
let rec sub l = function 0 -> [] | x -> hd l :: sub (tl l) (x - 1)

let no_dup l =
  let rec aux pref = function
    | [] -> List.rev pref
    | hd :: tl -> if List.mem hd pref then aux pref tl else aux (hd :: pref) tl
  in
  aux [] l

let rec count m = function
  | [] -> 0
  | x :: xs -> (if x = m then 1 else 0) + count m xs

let permutations l =
  let rec insert x lst =
    match lst with
    | [] -> [ [ x ] ]
    | h :: t -> (x :: lst) :: List.map (fun el -> h :: el) (insert x t)
  in

  let rec perm lst =
    match lst with
    | [] -> [ lst ]
    | h :: t -> List.flatten (List.map (insert h) (perm t))
  in
  perm l

let uncombine l =
  let rec aux a b = function
    | [] -> (List.rev a, List.rev b)
    | (a', b') :: tl -> aux (a' :: a) (b' :: b) tl
  in
  aux [] [] l
