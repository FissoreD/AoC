let parse l = List.hd l |> int_of_string

let add_string ar n v =
  if v < 10 then ar.(n) <- v
  else (
    ar.(n) <- v / 10;
    ar.(n + 1) <- v - 10);
  n + if v < 10 then 1 else 2

let main ~is_p1 cnt =
  let stop = if is_p1 then cnt + 10 else 30_000_000 in
  let ar = Array.make (stop + 30) ~-1 in
  let rec aux p1 p2 n =
    if n > stop then
      let build_str i = char_of_int (int_of_char '0' + ar.(i)) in
      let str = String.init n build_str in
      if is_p1 then String.sub str (stop - 10) 10
      else
        Re.Str.(search_forward (regexp (string_of_int cnt)) str) 0
        |> string_of_int
    else
      let n = add_string ar n (ar.(p1) + ar.(p2)) in
      let next_pos p = (p + 1 + ar.(p)) mod n in
      aux (next_pos p1) (next_pos p2) n
  in
  ar.(0) <- 3;
  ar.(1) <- 7;
  aux 0 1 2

let p1 l = main ~is_p1:true (parse l)
let p2 l = main ~is_p1:false (parse l)
