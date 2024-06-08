let switch grid x y = function
  | "+" -> grid.(x).(y) <- grid.(x).(y) + 1
  | "++" -> grid.(x).(y) <- grid.(x).(y) + 2
  | "-" -> grid.(x).(y) <- max 0 (grid.(x).(y) - 1)
  | "on" -> grid.(x).(y) <- 1
  | "off" -> grid.(x).(y) <- 0
  | "toggle" -> grid.(x).(y) <- 1 - grid.(x).(y)
  | _ -> invalid_arg "Invalid arg"

let parse_line l = 
  let parse_pair p = 
    let l = String.split_on_char ',' p |> List.map int_of_string in
    match l with [x;y] -> x,y | _ -> invalid_arg "invalid arg" in
  let l = String.split_on_char ' ' l in
  let l = if List.hd l = "turn" then List.tl l else l in
  (List.hd l), (List.nth l 1 |> parse_pair), (List.nth l 3 |> parse_pair)

let switch_grid grid (g, (x1, y1), (x2, y2)) =
  for i = x1 to x2 do
    for j = y1 to y2 do
      switch grid i j g
    done
  done

let p1 s = 
  let s = List.map parse_line s in
  let grid = Array.init 1000 (fun _ -> Array.init 1000 (fun _ -> 0)) in
  List.iter (switch_grid grid) s;
  Array.fold_left (Array.fold_left (+)) 0 grid |> string_of_int


let p2 s =   
  let s = List.map parse_line s in
  let s = List.map (function (a,b,c) -> (match a with "on" -> "+" | "off" -> "-" | _ -> "++"),b,c) s in
  let grid = Array.init 1000 (fun _ -> Array.init 1000 (fun _ -> 0)) in
  List.iter (switch_grid grid) s;
  Array.fold_left (Array.fold_left (+)) 0 grid |> string_of_int

