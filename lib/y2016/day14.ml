let parse = List.hd

let get_triple l =
  try
    Str.search_forward (Str.regexp {|\(.\)\1\1|}) l 0 |> ignore;
    Some (Str.matched_group 1 l).[0]
  with Not_found -> None

let get_five l =
  let rex = Printf.sprintf {|\(.\)\1\1\1\1|} in
  Utils.find_all rex l |> List.map (fun e -> e.[0])

let rec hash s = function
  | 0 -> s
  | n -> hash (Digest.string s |> Digest.to_hex) (Int.pred n)

let give_hash n salt key =
  let s = hash (Printf.sprintf "%s%d" salt key) n in
  (get_triple s, get_five s)

let next_mod_1000 = function 999 -> 0 | n -> Int.succ n

let is_key get pos =
  let has_five c p = List.mem c (get p |> snd) in
  match fst (get pos) with
  | None -> false
  | Some n ->
      let rec aux p =
        if p = pos then false
        else if has_five n p then true
        else aux (next_mod_1000 p)
      in
      aux (next_mod_1000 pos)

let find_keys hash =
  let ar = Array.init 1000 hash in
  let get i = Array.unsafe_get ar i in
  let rec aux apos found_nb pos =
    if found_nb = 64 then Int.pred pos
    else
      let found_nb = if is_key get apos then Int.succ found_nb else found_nb in
      Array.unsafe_set ar apos (hash (pos + 1000));
      aux (next_mod_1000 apos) found_nb (Int.succ pos)
  in
  aux 0 0 0

let p1 l = find_keys (give_hash 1 (parse l)) |> string_of_int
let p2 l = find_keys (give_hash 2017 (parse l)) |> string_of_int
