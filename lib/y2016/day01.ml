let parse_line l =
  let l = Str.(split (regexp ", ")) (List.hd l) in
  let split_str l =
    l.[0], int_of_string (String.sub l 1 (String.length l - 1))
  in List.map split_str l

let update_dir dir rot = (4 + if rot = 'R' then dir + 1 else dir - 1) mod 4

let update_pos (x,y) dir dist =
  if dir = 0 then x + dist, y else
  if dir = 1 then x, y + dist else
  if dir = 2 then x - dist, y else
  x, y - dist

let update (pos, dir) (rot, dist) =
  let dir = update_dir dir rot in
  let pos = update_pos pos dir dist in
  pos, dir

let p1 l =
  let l = parse_line l in
  let (x,y), _ = List.fold_left update ((0,0),0) l in
  abs x + abs y |> string_of_int

let rec walk seen l dir pos dist =
  if List.mem pos seen then pos else
  if dist = 0 then update_seen seen (pos, dir) l
  else
    let pos' = update_pos pos dir 1 in
    walk (pos :: seen) l dir pos' (dist - 1)

and
update_seen seen (pos, dir) l =
  if List.mem pos seen then pos else match l with
  | [] -> failwith "y16/d01 no pos visisted twice"
  | (rot, dist) :: tl -> walk seen tl (update_dir dir rot) pos dist


let p2 l =
  let l = parse_line l in
  let (x,y) = update_seen [] ((0,0), 0) l in
  abs x + abs y |> string_of_int

