let incr_ar ar pos =
  ar.(pos) <- ar.(pos) + 1;
  ar

let rec jumps ar c pos =
  if pos < 0 || pos >= Array.length ar then c
  else jumps (incr_ar ar pos) (c + 1) (pos + ar.(pos))

let p1 l = jumps (Array.of_list (List.map int_of_string l)) 0 0 |> string_of_int

let incr_ar ar pos =
  ar.(pos) <- (ar.(pos) + if ar.(pos) >= 3 then -1 else 1);
  ar

let rec jumps ar c pos =
  if pos < 0 || pos >= Array.length ar then c
  else jumps (incr_ar ar pos) (c + 1) (pos + ar.(pos))

let p2 l = jumps (Array.of_list (List.map int_of_string l)) 0 0 |> string_of_int
