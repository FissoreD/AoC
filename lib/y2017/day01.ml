let parse l = String.to_array (List.hd l)

let count ar step =
  let res, len = (ref 0, Array.length ar) in
  for i = 0 to len - 1 do
    if ar.(i) = ar.((i + step) mod len) then
      res := !res + int_of_char ar.(i) - int_of_char '0'
  done;
  !res

let p1 l = count (parse l) 1 |> string_of_int
let p2 l = count (parse l) (String.length (List.hd l) / 2) |> string_of_int
