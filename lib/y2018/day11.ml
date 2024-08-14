let hundreds n = n / 100 mod 10

let pow_lvl serial_nb (x, y) =
  let x = x + 10 in
  hundreds (((x * y) + serial_nb) * x) - 5

let pow_lvl_sum serial_nb pos =
  let center = Pos.add (1, 1) pos in
  center :: List.map (Pos.add center) Pos.neigh8
  |> List.map (pow_lvl serial_nb)
  |> List.sum

let p1 l =
  let m, serial_nb = (ref (0, (0, 0)), int_of_string (List.hd l)) in
  for y = 1 to 297 do
    for x = 1 to 297 do
      let pow_lvl = pow_lvl_sum serial_nb (x, y) in
      if fst !m < pow_lvl then m := (pow_lvl, (x, y))
    done
  done;
  Printf.sprintf "%d,%d" (fst (snd !m)) (snd (snd !m))

let add_row_col grid pos old_sum size =
  let get g (x, y) = g.(y).(x) in
  let sum = ref @@ get old_sum pos in
  for x = 0 to size do
    sum := !sum + get grid (Pos.add pos (size, x));
    sum := !sum + get grid (Pos.add pos (x, size))
  done;
  sum := !sum - get grid (Pos.add pos (size, size));
  !sum

let p2 l =
  let serial_nb = int_of_string (List.hd l) in
  let gsize = 300 in
  let grid =
    Array.(
      init (gsize + 1) (fun y ->
          init (gsize + 1) (fun x -> pow_lvl serial_nb (x, y))))
  in
  let grid_prov = Array.map Array.copy grid in
  let ps = ref (Array.map Array.max grid |> Array.max, (0, 0, 0)) in
  for size = 2 to gsize do
    for y = 1 to gsize - size do
      for x = 1 to gsize - size do
        let new_v = add_row_col grid (x, y) grid_prov (size - 1) in
        grid_prov.(y).(x) <- new_v;
        if new_v > fst !ps then ps := (new_v, (x, y, size))
      done
    done
  done;
  let a, b, c = snd !ps in
  Printf.sprintf "%d,%d,%d" a b c
