let parse l =
  let a, b, c = Utils.all_ints l |> Tuple.to3 in
  let x, y = ((a, a), (b, c)) in
  if l.[0] = 'x' then (x, y) else (y, x)

let parse = List.map parse

let rec size x y = function
  | [] -> (x + 3, y + 1)
  | ((_, b), (_, d)) :: tl -> size (max x b) (max y d) tl

let size = size 0 0

let build_grid grid ((x1, x2), (y1, y2)) =
  for x = x1 to x2 do
    grid.(y1).(x) <- '#'
  done;
  for y = y1 to y2 do
    grid.(y).(x1) <- '#'
  done

let build_grid grid = List.iter (build_grid grid)
let set grid pos c = grid.(snd pos).(fst pos) <- c

let rec fill_hor' grid dir pos =
  let next = Pos.add_dir dir pos in
  if Pos.valid_size (Array.length grid.(0), Array.length grid) next then
    match Pos.get grid next with
    | '|' when Pos.get grid (Pos.add_dir D next) = '|' -> ('.', next)
    | '.' when Pos.get grid (Pos.add_dir D next) = '.' -> ('.', next)
    | '|' -> fill_hor' grid dir next
    | '.' -> fill_hor' grid dir next
    | '#' -> ('#', pos)
    | '~' -> ('#', pos)
    | c -> Utils.error 2019 17 (Printf.sprintf "1: Unexpected %c char" c)
  else ('#', pos)

let rec fill_hor grid pos =
  let cR, pR = fill_hor' grid R pos in
  let cL, pL = fill_hor' grid L pos in
  let ch = if cL = '.' || cR = '.' then '|' else '~' in
  for x = fst pL to fst pR do
    set grid (x, snd pos) ch
  done;
  if cL = '.' then fill_down grid (Pos.add_dir D pL);
  if cR = '.' then fill_down grid (Pos.add_dir D pR);
  if ch = '~' && cL <> '|' && cR <> '|' then fill_hor grid (Pos.add_dir U pos)

and fill_down grid pos : unit =
  if Pos.valid_size (Array.length grid.(0), Array.length grid) pos then
    match Pos.get grid pos with
    | '.' ->
        set grid pos '|';
        fill_down grid (Pos.add_dir D pos)
    | '#' | '~' -> fill_hor grid (Pos.add_dir U pos)
    | '|' -> ()
    | c -> Utils.error 2019 17 (Printf.sprintf "2: Unexpected %c char" c)

let count grid c =
  let is_in e = if List.mem e c then 1 else 0 in
  let count arr = Array.fold_left (fun acc e -> acc + is_in e) 0 arr in
  Array.sum (Array.map count grid)

let main l ch =
  let l = parse l in
  let x, y = size l in
  let grid = Array.init y (fun _ -> Array.make x '.') in
  build_grid grid l;
  fill_down grid (500, 1);
  count grid ch |> string_of_int

let p1 l = main l [ '~'; '|' ]
let p2 l = main l [ '~' ]
