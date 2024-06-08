module SL = struct
  type t = string list
  let rec compare (a: t) (b: t) : int = match a, b with
    | [], [] -> 0
    | [], _ -> 1
    | _, [] -> -1  
    | x::xs, y::ys -> match String.compare x y with 
      | 0 -> compare xs ys | n -> n
end

module Set = Set.Make(SL)
module Map = Map.Make(String)

let str2dna = 
  let rex = Str.regexp {|\(e\|[A-Z][a-z]?\)|} in
  Misc.Utils.find_all rex

let parse l =
  let open List in
  let m = ref Map.empty in
  let add k v = m := Map.add k (v :: try Map.find k !m with Not_found -> []) !m in
  let l2tuple l =
    let split = String.split_on_char '-' l in
    add (hd split) (hd (tl split) |> str2dna)
  in 
  let rex = Str.regexp {|\(.*\) => \(.*\)|} in
  let target = hd l |> str2dna in
  let l1 = tl (tl l) |> map (Str.global_replace rex "\\1-\\2") in
  iter l2tuple l1;
  target, !m

let rec evolve_aux (add) (get: string -> SL.t list) pref = function
  | [] -> ()
  | hd :: tl ->
    (try List.iter (fun (x: SL.t) -> pref @ x @ tl |> add) (get hd);
     with Not_found -> ());
    evolve_aux add get (pref @ [hd]) tl

let evolve (map: string list list Map.t) l = 
  let set = ref Set.empty in
  let add s = set := Set.add s !set in
  let get s = Map.find s map in
  evolve_aux add get [] l;
  !set

let p1 l =
  let target, map = parse (List.rev l) in
  evolve map target |> Set.cardinal |> string_of_int

let rec is_prefix pref str = match pref,str with
  | [], suff -> true, suff
  | x :: xs, y :: ys when x = y -> is_prefix xs ys
  | _ -> false, str

exception STOP

let rec invole_aux add pref l k v = match l with
  | [] -> ()
  | l when k = "e" -> if l = v then raise STOP else ()
  | hd :: tl as l ->
    let same, suff = is_prefix v l in
    if same then add (List.rev_append pref (k :: suff));
    if List.length v <= List.length tl then
      invole_aux add (hd::pref) tl k v

let involve add keys values l =
  Array.iter2 (invole_aux add [] l) keys values

let involve_rec keys values l = 
  let acc = ref 0 in
  let to_explore = ref (Set.singleton l) in
  let seen = ref Set.empty in
  let add s v = 
    if not (Set.mem v !s) then s := Set.add v !s;
    seen := Set.add v !seen
  in
  try
    while true do
      incr acc;
      let set = ref Set.empty in
      Set.iter (involve (add set) keys values) !to_explore;
      to_explore := !set;
      Printf.printf "%d\n" (Set.cardinal !seen);
      assert (!acc < 3 && not (Set.is_empty !set))
    done; 0
  with STOP -> !acc

let p2 l =
  (* This is a valid code where the target is used to derive e. however it is
     slow due to combinatorial explosion... *)
  (* let target, map = parse (List.rev l) in
  let k, v = Map.fold (fun x -> List.fold_right (fun y (a,b) -> (x::a, y::b))) map ([], []) in
  involve_rec (Array.of_list k) (Array.of_list v) target |> string_of_int *)
  (* Here an alternative based on this htt3ps://www.reddit.com/r/adventofcode/comments/3xflz8/comment/cy4etju/ *)

  let count_Rn_Ar l = Misc.Utils.(count "Rn" l + count "Ar" l) in
  let count_Y = Misc.Utils.count "Y" in
  let target, _ = parse (List.rev l) in
  List.length target - count_Rn_Ar target - 2 * count_Y target - 1 |> string_of_int