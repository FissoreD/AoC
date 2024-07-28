(**
   returns all the substrings in s matching rex with overlapping
   for example, [find_all "a.a" "abaca"] returns [\["aba", "aca"\]]
*)
let find_all_overlap rex s =
  let rec aux p =
    try
      let x = Str.search_forward rex s p in
      let old_match = Str.matched_string s in
      old_match :: aux (x + 1)
    with Not_found -> []
  in
  aux 0

(**
   returns all the substrings in s matching rex (not overlapping)
   for example, [find_all "a.a" "abaca"] returns [\["aba"\]]
*)
let find_all rex s =
  let rex = Str.regexp rex in
  let rec aux p =
    try
      let x = Str.search_forward rex s p in
      let old_match = Str.matched_string s in
      old_match :: aux (x + String.length old_match)
    with Not_found -> []
  in
  aux 0

(** ax + b = 0  *)
let line_zero (a, b) = -.b /. a

(** ax² + bx + c = 0 → x = (-b ± √(b²-4ac))/2a  *)
let parabola_zeros (a, b, c) =
  let delta, den = ((b ** 2.) -. (4. *. a *. c), 2. *. a) in
  if den = 0. then [ line_zero (b, c) ]
  else if delta < 0. then []
  else if delta = 0. then [ -.b /. den ]
  else [ (-.b +. sqrt delta) /. den; (-.b -. sqrt delta) /. den ]

let time_it f =
  let t1 = Sys.time () in
  let res = f () in
  (Sys.time () -. t1, res)

(** return all ints in a string  *)
let all_ints ?(only_pos = false) s =
  let rex = (if only_pos then "" else "-?") ^ "[0-9]+" in
  find_all rex s |> List.map int_of_string

let is_int s =
  try
    let _ = int_of_string s in
    true
  with _ -> false

(** returns all the tuples with length nb_items summing up to max_sum

    for example:
      [all_combo ~max_sum:3 ~nb_items:2] gives [[[0 3] [1 2] [2 1] [3 0]]]
*)
let all_combo ~max_sum ~nb_items : int list list =
  let module MS = Set.Make (String) in
  let s = ref MS.empty in
  let add x = s := MS.add x !s in
  let add_l l = List.fold_left (Printf.sprintf "%s_%d") "" l |> add in
  let mem l = MS.mem (List.fold_left (Printf.sprintf "%s_%d") "" l) !s in

  let rec add_one_all (l : int list) : int list list =
    match l with
    | [] -> []
    | hd :: tl ->
        ((hd + 1) :: tl) :: List.map (fun x -> hd :: x) (add_one_all tl)
  in

  let distr_one (l : int list) : int list list =
    match l with
    | [] -> []
    | hd :: l -> List.map (fun x -> (hd - 1) :: x) (add_one_all l)
  in

  let res = ref [] in

  let rec repeat_build_list n (l : int list) =
    if n = -1 || mem l then ()
    else (
      add_l l;
      res := l :: !res;
      let ll = distr_one l in
      List.iter (repeat_build_list (n - 1)) ll)
  in

  let l = max_sum :: List.init (nb_items - 1) (fun _ -> 0) in
  repeat_build_list max_sum l;

  !res

(* same all_combo but returning a list of arrays *)
let all_combo_arr ~max_sum ~nb_items : int array list =
  let module MS = Set.Make (String) in
  let s = ref MS.empty in
  let add x = s := MS.add x !s in
  let add_l l = Array.fold_left (Printf.sprintf "%s_%d") "" l |> add in
  let mem l = MS.mem (Array.fold_left (Printf.sprintf "%s_%d") "" l) !s in

  let add_one_all hd (l : int array) : int array array =
    let res =
      Array.init
        (Array.length l - 1)
        (fun x ->
          Array.init (Array.length l) (fun n ->
              if n = 0 then hd else if n = x + 1 then l.(n) + 1 else l.(n)))
    in
    res
  in

  let distr_one (l : int array) : int array array =
    if Array.length l = 0 then Array.make 0 (Array.make 0 0)
    else add_one_all (l.(0) - 1) l
  in

  let res = ref [] in

  let rec repeat_build_list n (l : int array) =
    if n = -1 || mem l then ()
    else (
      add_l l;
      res := l :: !res;
      let ll = distr_one l in
      Array.iter (repeat_build_list (n - 1)) ll)
  in

  let l = Array.init nb_items (function 0 -> max_sum | _ -> 0) in
  repeat_build_list max_sum l;

  !res

exception InvalidInput of int * int * string

let error year day msg = raise @@ InvalidInput (year, day, msg)
let dec25_p2 = Printf.sprintf "END %d!"
