let give_hash pref i = Day10.p2 [ Printf.sprintf "%s-%d" pref i ]

let str2grid_line l =
  let char2bit i =
    let char = Printf.sprintf "%c" l.[i / 4] in
    Scanf.sscanf char "%x" (fun x -> (x lsr (3 - (i mod 4))) land 1)
  in
  Array.init (String.length l * 4) char2bit

let build_disk l = Array.init 128 (fun i -> give_hash l i |> str2grid_line)
let p1 l = Array.(map sum (build_disk (List.hd l)) |> sum) |> string_of_int

let give_sets mat =
  let pos2int (x, y) = (y * 128) + x in
  let is_valid p = Pos.valid_size (128, 128) p && Pos.get mat p <> 0 in
  let give_value i = if Pos.get mat (i mod 128, i / 128) = 0 then -2 else -1 in
  let sets = Array.init (128 * 128) give_value in
  let rec find p = if sets.(p) = -1 then p else find sets.(p) in
  let union p1 p2 =
    let f1, f2 = (find (pos2int p1), find (pos2int p2)) in
    if f1 <> f2 then sets.(find (pos2int p1)) <- find (pos2int p2)
  in
  for y = 0 to 127 do
    for x = 0 to 127 do
      if Pos.get mat (x, y) = 1 then
        let neigh = List.map (Pos.add (x, y)) Pos.dir4 in
        List.iter (union (x, y)) (List.filter is_valid neigh)
    done
  done;
  Array.count (-1) sets

let p2 l = give_sets (build_disk (List.hd l)) |> string_of_int
