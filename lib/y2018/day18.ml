type tyle = Ground | Tree | Lumberyard

let to_ch = function Ground -> '.' | Tree -> '|' | Lumberyard -> '#'

let parse_char = function
  | '.' -> Ground
  | '|' -> Tree
  | '#' -> Lumberyard
  | _ -> raise (Invalid_argument "Unkown char")

let parse l =
  List.map (fun s -> Array.map parse_char @@ String.to_array s) l
  |> Array.of_list

let neigh grid pos =
  let n = List.map (Pos.add pos) Pos.dir8 in
  List.filter (Pos.valid_arr grid) n

let copy g1 g2 =
  Array.iteri (fun x e -> Array.iteri (fun y v -> g2.(x).(y) <- v) e) g1

let step g1 g2 =
  Array.iteri
    (fun y e ->
      Array.iteri
        (fun x v ->
          let neigh = List.map (Pos.get g1) (neigh g1 (x, y)) in
          g2.(y).(x) <-
            (match v with
            | Ground -> if List.count Tree neigh >= 3 then Tree else Ground
            | Tree ->
                if List.count Lumberyard neigh >= 3 then Lumberyard else Tree
            | Lumberyard ->
                if
                  List.count Lumberyard neigh >= 1 && List.count Tree neigh >= 1
                then Lumberyard
                else Ground))
        e)
    g1

let tot g =
  Array.sum (Array.map (Array.count Tree) g)
  * Array.sum (Array.map (Array.count Lumberyard) g)

let print_grid =
  Array.iter (fun x ->
      Array.iter (fun e -> Printf.printf "%c" (to_ch e)) x;
      Printf.printf "\n%!")

let p1 l =
  let g1 = parse l in
  let g2 = parse l in
  let rec repeat n g1 g2 =
    if n = 0 then tot g1
    else (
      step g1 g2;
      repeat (n - 1) g2 g1)
  in
  repeat 10 g1 g2 |> string_of_int

let p2 l =
  let g1 = parse l in
  let g2 = parse l in
  let tbl = Hashtbl.create 2025 in
  let it = 1_000_000_000 in
  let rec repeat n g1 g2 =
    Format.printf "It %d\n%!" n;
    print_grid g1;
    Format.printf "\n%!";
    if n = 0 then tot g1
    else if Hashtbl.mem tbl g1 then (
      Format.printf "Found %d\n%!" (Hashtbl.find tbl g1);
      failwith "STOP")
    else (
      step g1 g2;
      Hashtbl.add tbl g1 (it - n);
      repeat (n - 1) g2 g1)
  in
  repeat it g1 g2 |> string_of_int
