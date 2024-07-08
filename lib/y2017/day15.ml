let nextA n = n * 16807 mod 2_147_483_647
let nextB n = n * 48271 mod 2_147_483_647

let same a b =
  let mask = (2 lsl 15) - 1 in
  if a land mask = b land mask then 1 else 0

let parse l = List.map (fun i -> List.hd (Utils.all_ints i)) l |> Tuple.to2

let rec repeat fa fb acc (a, b) = function
  | 0 -> acc + same a b
  | n -> repeat fa fb (same a b + acc) (fa a, fb b) (n - 1)

let p1 l = repeat nextA nextB 0 (parse l) 40_000_000 |> string_of_int

let rec nextA' n =
  let r = n * 16807 mod 2_147_483_647 in
  if r mod 4 = 0 then r else nextA' r

let rec nextB' n =
  let r = n * 48271 mod 2_147_483_647 in
  if r mod 8 = 0 then r else nextB' r

let p2 l = repeat nextA' nextB' 0 (parse l) 5_000_000 |> string_of_int
