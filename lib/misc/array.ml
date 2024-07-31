include Array

exception ArrayIndexOutBound

let index_of ar x =
  try
    let pos = ref 0 in
    while ar.(!pos) <> x do
      incr pos
    done;
    !pos
  with Invalid_argument _ -> raise ArrayIndexOutBound

let swap ar p1 p2 =
  let x = ar.(p1) in
  ar.(p1) <- ar.(p2);
  ar.(p2) <- x

let swap_safe ar p1 p2 =
  let len = Array.length ar in
  swap ar (p1 mod len) (p2 mod len)

let sum = Array.fold_left ( + ) 0
let count m = Array.fold_left (fun acc e -> acc + if e = m then 1 else 0) 0
let max ?(m = min_int) = Array.fold_left max m
let min ?(m = max_int) = Array.fold_left min m
