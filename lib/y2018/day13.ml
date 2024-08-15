type next_dir = unit -> Pos.dir4 -> Pos.dir4
type cart = { pos : Pos.t; dir : Pos.dir4; next_dir : next_dir }

let give_dir () =
  let dir = ref (-1) in
  let dirs = [| Pos.rot_left; Fun.id; Pos.rot_right |] in
  fun () ->
    dir := (!dir + 1) mod 3;
    dirs.(!dir)

let parse l =
  let l = List.map String.to_list l in
  let is_cart y x e =
    try Some { dir = Pos.ch2dir e; pos = (x, y); next_dir = give_dir () }
    with _ -> None
  in
  let grid = List.map Array.of_list l |> Array.of_list in
  let pos = List.(mapi (fun y -> mapi (is_cart y)) l) in
  (grid, List.filter_map Fun.id (List.flatten pos))

let move grid cart =
  let dir =
    match Pos.get grid cart.pos with
    | '+' -> cart.next_dir () cart.dir
    | '/' -> ( match cart.dir with U -> R | D -> L | L -> D | R -> U)
    | '\\' -> ( match cart.dir with U -> L | D -> R | L -> U | R -> D)
    | _ -> cart.dir
  in
  { cart with dir; pos = Pos.add_dir dir cart.pos }

let sort carts =
  let compare { pos = x1, y1 } { pos = x2, y2 } =
    if y1 = y2 then compare x1 x2 else compare y1 y2
  in
  List.sort compare carts

let move_all grid carts =
  let collide c1 c2 = c1.pos = c2.pos in
  let rec aux ncarts = function
    | [] -> (None, sort ncarts)
    | cart :: carts ->
        let cart = move grid cart in
        let coll1, ncarts = List.partition (collide cart) ncarts in
        let coll2, carts = List.partition (collide cart) carts in
        if coll1 = [] && coll2 = [] then aux (cart :: ncarts) carts
        else
          let _, carts = aux ncarts carts in
          (Some cart.pos, carts)
  in
  aux [] carts

let p1 l =
  let grid, carts = parse l in
  let rec aux carts =
    let p, carts' = move_all grid carts in
    match p with
    | None -> aux carts'
    | Some (x, y) -> Printf.sprintf "%d,%d" x y
  in
  aux carts

let p2 l =
  let grid, carts = parse l in
  let rec aux carts =
    let _, carts' = move_all grid carts in
    match carts' with
    | [] -> Utils.error 2018 13 ""
    | [ { pos = x, y } ] -> Printf.sprintf "%d,%d" x y
    | _ -> aux carts'
  in
  aux carts
