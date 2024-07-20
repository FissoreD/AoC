let parse l = Str.split (Str.regexp " => ") l |> Tuple.to2
let to_str s = Array.map String.of_array s |> Array.fold_left ( ^ ) ""
let to_mat s = Array.of_list String.(split_on_char '/' s |> List.map to_array)

let rotate_square_mat mat =
  let len = Array.length mat in
  Array.(init len (fun j -> init len (fun i -> mat.(len - i - 1).(j))))

let flip mat =
  let len = Array.length mat in
  Array.(init len (fun j -> init len (fun i -> mat.(j).(len - i - 1))))

let all_rotations rules =
  let tbl = Hashtbl.create 1024 in
  let rotate (x, y) =
    let value = to_mat y in
    let a = rotate_square_mat (to_mat x) in
    let b = rotate_square_mat a in
    let c = rotate_square_mat b in
    let rots = [ to_mat x; a; b; c ] in
    let rots = rots @ List.map flip rots in
    List.iter (fun e -> Hashtbl.replace tbl (to_str e) value) rots
  in
  List.iter rotate rules;
  tbl

let update rules mat =
  let size = Array.length mat in
  let den = if Int.is_even size then 2 else 3 in
  let build_line x y =
    let res = ref "" in
    for i = 0 to den - 1 do
      for j = 0 to den - 1 do
        res := Printf.sprintf "%s%c" !res mat.((den * x) + i).((den * y) + j)
      done
    done;
    !res
  in
  let side = size / den in
  let size' = side * (den + 1) in
  let _mat = Array.init size' (fun _ -> Array.make size' '.') in
  for i = 0 to side - 1 do
    for j = 0 to side - 1 do
      let l' = Hashtbl.find rules (build_line i j) in
      for x = 0 to den do
        for y = 0 to den do
          _mat.((i * (den + 1)) + x).((j * (den + 1)) + y) <- l'.(x).(y)
        done
      done
    done
  done;
  _mat

let rec update_iter rules mat = function
  | 0 -> Array.sum (Array.map (Array.count '#') mat)
  | n -> update_iter rules (update rules mat) (n - 1)

let main n l =
  let mat = to_mat ".#./..#/###" in
  update_iter (List.map parse l |> all_rotations) mat n |> string_of_int

let p1 = main 5
let p2 = main 18
