let all_primes_sum ?(max=None) n =
  let acc = ref 0 in
  let max = Option.value ~default:(int_of_float (sqrt (float n))) max in
  for i = 1 to max do
    if n mod i = 0 then
      let y = let x = n / i in if x = i then 0 else x in
      acc := !acc + i + y
  done;
  !acc

let rec get_value ?(max=None) ~coeff tgt n =
  if all_primes_sum ~max n * coeff >= tgt then n
  else get_value ~max ~coeff tgt (n + 1)

let p1 n =
  (List.hd n |> int_of_string |> get_value ~coeff:10) 1 |> string_of_int

let p2 n =
  (List.hd n |> int_of_string |> get_value ~max:(Some 50) ~coeff:11) 1 |> string_of_int
