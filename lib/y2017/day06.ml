let find_max_pos ar =
  let len = Array.length ar in
  let keep_max p1 p2 = if ar.(p1) >= ar.(p2) then p1 else p2 in
  let rec aux pm p = if p >= len then pm else aux (keep_max pm p) (p + 1) in
  aux 0 0

let distrib ar p =
  let len = Array.length ar in
  let rec repeat n p =
    if n = 0 then ()
    else (
      ar.(p) <- ar.(p) + 1;
      repeat (n - 1) ((p + 1) mod len))
  in
  let n = ar.(p) in
  ar.(p) <- 0;
  repeat n ((p + 1) mod len)

type t = int array [@@deriving show]

let distrib_all is_p1 l =
  let ar = Array.of_list (Utils.all_ints (List.hd l)) in
  let mem = Hashtbl.create 1024 in
  let rec aux cnt ar =
    if Hashtbl.mem mem ar then if is_p1 then cnt else cnt - Hashtbl.find mem ar
    else (
      Hashtbl.add mem ar cnt;
      let ar = Array.copy ar in
      distrib ar (find_max_pos ar);
      aux (cnt + 1) ar)
  in
  aux 0 ar |> string_of_int

let p1 l = distrib_all true l
let p2 l = distrib_all false l
