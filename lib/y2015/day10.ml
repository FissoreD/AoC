let update_once (len, l) =
  let posl, occ_nb = ref 1, ref 1 in
  let get_old () = l.(!posl - 1) in

  let newl, pos_newl = Array.make (len * 2) ~-1, ref 0 in

  let add_newl e = newl.(!pos_newl) <- e; incr pos_newl in
  let update_newl () = add_newl !occ_nb; add_newl (get_old ()); occ_nb := 1 in

  while !posl < len && l.(!posl) <> ~-1 do
    (if l.(!posl) = get_old () then incr occ_nb else update_newl ());
    incr posl
  done;
  update_newl ();
  !pos_newl, newl

let build_look_and_say l n =
  let l = List.hd l in
  let len = String.length l in
  let l = Array.init len (fun i -> int_of_char l.[i] - int_of_char '0') in
  let res = ref (len, l) in
  for _ = 1 to n do res := update_once !res done;
  fst !res

let p1 l = build_look_and_say l 40 |> string_of_int

let p2 l = build_look_and_say l 50 |> string_of_int
