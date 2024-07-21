module M = Map.Make (Pos)

let parse l =
  let m = Hashtbl.create 1024 in
  List.iteri (fun y -> String.iteri (fun x e -> Hashtbl.add m (x, y) e)) l;
  let len = List.length l in
  (m, (String.length (List.hd l) / 2, len / 2), Pos.U)

let triple grid pos dir e f =
  let p = f dir in
  Hashtbl.replace grid pos e;
  (grid, Pos.add_dir p pos, p)

let update (n, (grid, pos, dir)) =
  let triple = triple grid pos dir in
  match Hashtbl.find_opt grid pos |> Option.value ~default:'.' with
  | '#' -> (n, triple '.' Pos.rot_right)
  | _ -> (n + 1, triple '#' Pos.rot_left)

let p1 l =
  let rec aux a = function 0 -> a | n -> aux (update a) (n - 1) in
  aux (0, parse l) 10000 |> fst |> string_of_int

let update' (n, (grid, pos, dir)) =
  let triple = triple grid pos dir in
  match Hashtbl.find_opt grid pos |> Option.value ~default:'.' with
  | '.' -> (n, triple 'w' Pos.rot_left)
  | 'w' -> (n + 1, triple '#' Fun.id)
  | '#' -> (n, triple 'f' Pos.rot_right)
  | _ -> (n, triple '.' Pos.rot_inv)

let p2 l =
  let rec aux a = function 0 -> a | n -> aux (update' a) (n - 1) in
  aux (0, parse l) 10_000_000 |> fst |> string_of_int
