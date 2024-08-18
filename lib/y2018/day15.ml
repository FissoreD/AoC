type unit_info = { mutable hp : int; mutable pos : Pos.t; ap : int; t : char }

let parse l = List.map String.to_array l |> Array.of_list

let get_units ~elf_ap grid =
  let units = ref [] in
  let get_ap t = if t = 'G' then 3 else elf_ap in
  let build_unit pos t = { hp = 200; ap = get_ap t; pos; t } in
  let add_unit x y e =
    if e = 'G' || e = 'E' then units := build_unit (x, y) e :: !units
  in
  Array.iteri (fun y -> Array.iteri (fun x -> add_unit x y)) grid;
  !units

exception STOP
exception KILL_ELF

let obstacles = [ '#'; 'G'; 'E' ]
let sort_units = List.sort (fun x y -> Pos.compare x.pos y.pos)
let get_neigh p = List.map (Pos.add p) Pos.dir4
let filter_free grid = List.filter (fun pos -> Pos.get grid pos = '.')
let set grid pos v = grid.(snd pos).(fst pos) <- v

let set_reset grid pos v =
  let old_v, set = (Pos.get grid pos, set grid pos) in
  set v;
  fun () -> set old_v

let bfs grid p1 dests =
  let reset1, l = (set_reset grid p1 '.', ref []) in
  let continue Bfs.{ to_exp } =
    l := List.filter (fun e -> List.mem e to_exp) dests;
    !l <> []
  in
  Bfs.bfs Pos.dir4 obstacles grid continue p1 |> ignore;
  reset1 ();
  if !l = [] then None else Some (List.hd (Pos.sort_list !l))

let move grid enemies unit =
  let neigh = filter_free grid (get_neigh unit.pos) in
  let in_range = List.(map (fun e -> get_neigh e.pos) enemies |> flatten) in
  if neigh <> [] && not (List.mem unit.pos in_range) then
    Option.iter
      (fun nearest ->
        let new_pos = Option.get (bfs grid nearest neigh) in
        set grid unit.pos '.';
        set grid new_pos unit.t;
        unit.pos <- new_pos)
      (bfs grid unit.pos (filter_free grid in_range))

let attack is_p1 grid units unit =
  let neigh = get_neigh unit.pos in
  let is_to_attack e = e.t <> unit.t && List.mem e.pos neigh in
  let to_attack = List.filter is_to_attack units in
  let min = List.fold_left (fun acc u -> min acc u.hp) max_int to_attack in
  Option.iter
    (fun u ->
      u.hp <- u.hp - unit.ap;
      if (not is_p1) && u.hp <= 0 && u.t = 'E' then raise KILL_ELF;
      if u.hp <= 0 then set grid u.pos '.')
    (List.find_opt (fun u -> u.hp = min) (sort_units to_attack))

let move_attack is_p1 grid units unit =
  if unit.hp > 0 then (
    let get_enemies unit = List.filter (fun x -> x.t <> unit.t && x.hp > 0) in
    let enemies = get_enemies unit units in
    if enemies = [] then raise STOP;
    move grid enemies unit;
    attack is_p1 grid enemies unit)

let rec act is_p1 cnt_step grid units =
  let units = List.filter (fun x -> x.hp > 0) units in
  try
    List.iter (move_attack is_p1 grid units) (sort_units units);
    act is_p1 (cnt_step + 1) grid units
  with STOP -> (cnt_step, List.filter (fun x -> x.hp > 0) units)

let main is_p1 l elf_ap =
  let grid = parse l in
  act is_p1 0 grid (get_units ~elf_ap grid)

let get_res (a, b) = List.sum (List.map (fun x -> x.hp) b) * a |> string_of_int
let p1 l = get_res (main true l 3)

let p2 l =
  let rec no_losses elf_ap =
    try get_res (main false l elf_ap) with KILL_ELF -> no_losses (elf_ap + 1)
  in
  no_losses 4
