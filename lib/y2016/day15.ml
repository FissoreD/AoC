let enter_disc (nb, size, pos) = (size - ((nb + pos) mod size), size)

let parse l =
  match Utils.all_ints l with
  | [ n; size; _; pos ] -> (n, size, pos) (* Ignore time since always 0 *)
  | _ -> Utils.error 2016 15 ""

let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let lcm a b = a * b / gcd a b

let rec align (m, sm) (n, sn) =
  if m = n then (n, lcm sm sn)
  else if m > n then align (m, sm) (n + sn, sn)
  else align (m + sm, sm) (n, sn)

let align_all l =
  let l = List.map enter_disc l in
  let rec align_l p = function
    | [] -> fst p
    | hd :: tl -> align_l (align p hd) tl
  in
  align_l (List.hd l) (List.tl l)

let p1 l = List.map parse l |> align_all |> string_of_int

let p2 l =
  let l = List.map parse l @ [ (7, 11, 0) ] in
  align_all l |> string_of_int
