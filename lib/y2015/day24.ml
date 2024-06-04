let prod = List.fold_left Int.mul 1
let sum = List.fold_left Int.add 0
let max_info = (max_int, max_int)
let to_min_info l = (List.length l, prod l)

let min_option ((a, b) as m) ((c, d) as n) =
  if c < a then n else if c = a then (a, Int.min b d) else m

let min_len m l1 l2 =
  List.fold_left min_option m [ to_min_info l1; to_min_info l2 ]

let rec_case f min goal acc pref1 pref2 = function
  | [] -> min
  | hd :: tl ->
      let h1 = f min goal (acc + hd) (hd :: pref1) pref2 tl in
      f h1 goal acc pref1 (hd :: pref2) tl

let stop (mlen, qe) l =
  let len = List.length l in
  mlen < len || (mlen = len && qe < prod l)

let rec split2 min goal acc pref1 pref2 suff =
  if stop min pref1 || acc > goal then min
  else if goal = acc then min_len min pref1 (pref2 @ suff)
  else rec_case split2 min goal acc pref1 pref2 suff

let rec splitn f min goal acc pref1 pref2 suff =
  if stop min pref1 || acc > goal then min
  else if acc = goal then
    let min1 = f max_info goal 0 [] [] (pref2 @ suff) in
    min_option min1 min |> min_option (to_min_info pref1)
  else rec_case (splitn f) min goal acc pref1 pref2 suff

let split3 = splitn split2

let p1 l =
  let l = List.map int_of_string l in
  let goal = sum l / 3 in
  split3 max_info goal 0 [] [] l |> snd |> print_int

let split4 = splitn split3

let p2 l =
  let l = List.map int_of_string l in
  let goal = sum l / 4 in
  split4 max_info goal 0 [] [] l |> snd |> print_int
