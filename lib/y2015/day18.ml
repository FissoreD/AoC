let neigh_sum l x y =
  let tot = ref 0 in
  for i = max 0 (x - 1) to min (x + 1) (Array.length l - 1) do
    for j = max 0 (y - 1) to min (y + 1) (Array.length l.(0) - 1) do
      if i = x && j = y then ()
      else tot := !tot + l.(i).(j)
    done;
  done;
  !tot

let evolve (old_arr, new_arr) to_one = 
  for i = 0 to Array.length old_arr - 1 do
    for j = 0 to Array.length old_arr.(0) - 1 do
      if List.mem (i,j) to_one then new_arr.(i).(j) <- 1
      else
        let light_on = neigh_sum old_arr i j in
        let v = old_arr.(i).(j) in
        let cond = (v = 1 && (light_on > 3 || light_on < 2) || (v = 0 && light_on = 3)) in
        new_arr.(i).(j) <- if cond then (1 - v) else v
    done
  done

let main l to_one = 
  let l = Array.of_list l in
  let l = Array.init (Array.length l) 
    (fun i -> Array.init (String.length l.(0)) (fun j -> if l.(i).[j] = '#' then 1 else 0)) in
  List.iter (fun (x, y) -> l.(x).(y) <- 1) to_one;
  let new_arr = Array.init (Array.length l) (fun _ -> Array.make (Array.length l.(0)) 0) in
  let state = ref (l,new_arr) in
  for _ = 1 to 100 do
    evolve !state to_one;
    state := snd !state, fst !state
  done;
  Array.fold_left (Array.fold_left (+)) 0 (fst !state) |> print_int


let p1 l = main l []

let p2 l = 
  let width = List.length l in
  let height = List.hd l |> String.length in
  let to_one = [0,0; width - 1, 0; 0, height - 1; width - 1, height - 1] in
  main l to_one
