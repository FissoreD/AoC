let parse = List.map (fun l -> Utils.all_ints l |> Tuple.to2)
let dist (a, b) (c, d) = abs (a - c) + abs (d - b)

let find_nearest pos =
  let rec aux index ((mdist, mindex, count) as m') = function
    | [] -> if count > 1 then -1 else mindex
    | x :: xs ->
        let dist = dist pos x in
        let min =
          if dist < mdist then (dist, index, 1)
          else if dist = mdist then (mdist, mindex, 2)
          else m'
        in
        aux (index + 1) min xs
  in
  aux 0 (max_int, -1, 0)

let p1 l =
  let l = parse l in
  let xs, ys = (List.map fst l, List.map snd l) in
  let minX, minY = (List.min xs, List.min ys) in
  let maxX, maxY = (List.max xs, List.max ys) in
  let tbl = Hashtbl.create 1024 in
  for y = minY to maxY do
    for x = minX to maxX do
      let e = find_nearest (x, y) l in
      if e = -1 then ()
      else if x = minX || x = maxX || y = minY || y = maxY then
        Hashtbl.replace tbl e infinity
      else
        let old_v = Hashtbl.find_opt tbl e |> Option.value ~default:0. in
        Hashtbl.replace tbl e (old_v +. 1.)
    done
  done;
  let res = Hashtbl.to_seq_values tbl in
  Seq.(filter (( <> ) infinity) res |> fold_left max 0.)
  |> int_of_float |> string_of_int

let p2 l =
  let l = parse l in
  let x, y = List.hd l in
  let max_dist = 10000 in
  let bound = max_dist / List.length l in
  let minX, minY = (x - bound, y - bound) in
  let maxX, maxY = (x + bound, y + bound) in
  let cnt = ref 0 in
  for y = minY to maxY do
    for x = minX to maxX do
      if List.sum (List.map (dist (x, y)) l) < max_dist then incr cnt
    done
  done;
  string_of_int !cnt
