let c2i c = int_of_char c.[0] - int_of_char 'a'

let parse l =
  let get ar n = if Utils.is_int n then int_of_string n else ar.(c2i n) in
  let binary ar i op v x = snd ((ar.(i) <- op ar.(i) v), (1, x)) in
  match String.split_on_char ' ' l with
  | [ "set"; c; n ] -> fun ar -> snd ((ar.(c2i c) <- get ar n), (1, 0))
  | [ "mul"; c; n ] -> fun ar -> binary ar (c2i c) ( * ) (get ar n) 1
  | [ "sub"; c; n ] -> fun ar -> binary ar (c2i c) ( - ) (get ar n) 0
  | [ "jnz"; n; m ] -> fun ar -> ((if get ar n <> 0 then get ar m else 1), 0)
  | _ -> Utils.error 2017 18 l

let build_ar () = Array.make (c2i "h" + 1) 0

let rec run l ar (pos, acc) =
  if pos >= Array.length l || pos < 0 then acc
  else run l ar (Pos.add (pos, acc) (l.(pos) ar))

let p1 l =
  let l = Array.map parse (Array.of_list l) in
  run l (build_ar ()) (0, 0) |> string_of_int

let p2 l =
  (* Given an interval [x;y],
     counts the numbers of number in [x;x+17;x+17*2...;y] that are not prime *)
  let l = Array.map parse (Array.sub (Array.of_list l) 0 8) in
  let mem = build_ar () in
  mem.(0) <- 1;
  run l mem (0, 0) |> ignore;
  let x = ref mem.(c2i "b") in
  let y = ref mem.(c2i "c") in
  let res = ref 0 in
  while !x <= !y do
    if not (Int.is_prime !x) then incr res;
    x := !x + 17
  done;
  string_of_int !res

(*
  Below the steps of the program simplification:
  STEP 1:
    B = 57 * 100 + 100000
    C = B + 17000
    DO A:
      F = 1
      D = 2
      DO B:
        E = 2
        DO LOOP3:
          G = D * E - B
          IF G = 0 then (set f 0)
          E += 1
          G = E - B
        IF G <> 0 GOTO LOOP3
        d += 1
        g = d - b
      IF G <> 0 GOTO B
      if f = 0 then (sub h -1)
      g = b - c
      b += 17
    IF G <> 0 GOTO A

  STEP 2:
    B = 57 * 100 + 100000
    C = B + 17000
    DO A:
      F = 1
      D = 2
      DO B:
        FOR E = 2 TO B - 1 do:
          G = D * E - B
          IF G = 0 then (F = 0; break)
        d += 1
        g = d - b
      IF G <> 0 GOTO B
      if f = 0 then (sub h -1)
      g = b - c
      b += 17
    IF G <> 0 GOTO A

  STEP 3:
    B = 57 * 100 + 100000
    C = B + 17000
    DO A:
      F = 1
      FOR D = 2 to B - 1 do:
        FOR E = 2 TO B - 1 do:
          G = D * E - B
          IF G = 0 then (F = 0; break all)
      if f = 0 then (h += 1)
      g = b - c
      b += 17
    IF G <> 0 GOTO A

  STEP 4:
    B = 57 * 100 + 100000
    C = B + 17000
    FOR (; B <= C; B += 17):
      F = 1
      FOR D = 2 to B - 1 do:
        FOR E = 2 TO B - 1 do:
          IF D * E = B then (F = 0; break all)
      if f = 0 then (h += 1)

    B = 57 * 100 + 100000
    C = B + 17000
    FOR (; B <= C; B += 17):
      FOR D = 2 to B - 1 do:
        FOR E = 2 TO B - 1 do:
          IF D * E = B then (H += 1; break all)

  STEP 5:
    B = 57 * 100 + 100000
    C = B + 17000
    FOR (; B <= C; B += 17):
      FOR D = 2 to B - 1 do:
        IF B mod D = 0 then (H += 1; break all)

  STEP 6:
    B = 57 * 100 + 100000
    C = B + 17000
    FOR (; B <= C; B += 17):
      IF not (prime B) then (H += 1; break all)

    PROG: count not primes between B and C
*)
