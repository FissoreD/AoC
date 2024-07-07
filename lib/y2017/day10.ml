let reverse pos len ar =
  for i = 0 to (len / 2) - 1 do
    Array.swap_safe ar (pos + i) (pos + len - 1 - i)
  done

let update len ar pos ssize =
  reverse pos len ar;
  (pos + ssize + len) mod Array.length ar

let update lens ar n =
  let len = Array.length lens in
  let rec round ssize pos rep = function
    | 0 -> repeat ssize pos (rep - 1)
    | n -> round (ssize + 1) (update lens.(len - n) ar pos ssize) rep (n - 1)
  and repeat ssize pos = function 0 -> () | n -> round ssize pos n len in
  repeat 0 0 n

let main l parser repeat concl =
  let l = parser (List.hd l) |> Array.of_list in
  let ar = Array.init 256 Fun.id in
  update l ar repeat;
  concl ar

let p1 l = main l Utils.all_ints 1 (fun ar -> ar.(0) * ar.(1) |> string_of_int)

let str2ascii s =
  let l = String.to_list s |> List.map int_of_char in
  l @ [ 17; 31; 73; 47; 23 ]

let hash ar =
  let add e = Printf.sprintf "%.2x" e in
  let get i j = ar.((i * 16) + j) in
  let rec big_xor i = function
    | 0 -> get i 0
    | j -> get i j lxor big_xor i (j - 1)
  and aux = function 16 -> "" | n -> add (big_xor n 15) ^ aux (n + 1) in
  aux 0

let p2 l = main l str2ascii 64 hash
