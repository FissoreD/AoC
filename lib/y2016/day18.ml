let parse l =
  (* . = true | ^ = false *)
  let l, len = (List.hd l, String.length (List.hd l)) in
  let a1 = Array.init len (fun i -> l.[i] = '.') in
  let a2 = Array.make len false in
  (a1, a2)

let is_trap ar pos =
  let l = pos = 0 || ar.(pos - 1) in
  let r = pos = Array.length ar - 1 || ar.(pos + 1) in
  l <> r

let update (a1, a2) =
  for i = 0 to Array.length a1 - 1 do
    a2.(i) <- not (is_trap a1 i)
  done

let count_true = Array.fold_left (fun acc x -> acc + if x then 1 else 0) 0

let update n a =
  let swap (a, b) = (b, a) in
  let x = ref a in
  let res = ref (count_true (fst a)) in
  for _ = 2 to n do
    update !x;
    x := swap !x;
    res := !res + count_true (fst !x)
  done;
  !res |> string_of_int

let p1 l = update 40 (parse l)
let p2 l = update 400000 (parse l)
