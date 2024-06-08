let c2i c = int_of_char c - int_of_char '0'

let next_digest pref suff =
  Digest.(string (Printf.sprintf "%s%d" pref suff) |> to_hex)

let valid_digest d =
  let ok = ref true in
  let pos = ref 0 in
  while !ok && !pos < 5 do
    if d.[!pos] <> '0' then ok := false;
    incr pos
  done;
  !ok

let find_next ~valid_digest pref =
  let n = ref 0 in
  let rec aux () =
    let x = next_digest pref !n in
    incr n;
    if valid_digest x then x else aux ()
  in
  aux

let build_psw ~valid_digest ~f ~loop pref =
  let ar = Array.make 8 '_' in
  let generator = find_next ~valid_digest pref in
  loop ar (fun () -> (f ar) (generator ()));
  String.init 8 (Array.get ar)

let f =
  let pos = ref 0 in
  fun ar s -> ar.(!pos) <- s.[5]; incr pos

let loop _ f = for _ = 1 to 8 do f () done

let p1 s = build_psw ~f ~loop ~valid_digest (List.hd s) 

let valid_digest d =
  let x = c2i d.[5] in
  x >= 0 && x <= 7 && valid_digest d

let f ar s =
  let pos = c2i s.[5] in
  if ar.(pos) = '_' then ar.(pos) <- s.[6]

let rec loop ar f : unit =
  f (); if Array.mem '_' ar then loop ar f

let p2 s = build_psw ~f ~loop ~valid_digest (List.hd s) 
