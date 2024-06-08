let init, coeff, div = (20151125, 252533, 33554393)

let pos l =
  match Utils.all_ints (List.hd l) with
  | [ x; y ] -> (x, y)
  | _ -> failwith "y15/d25: invalid entry"


let pos2int x y = 
  let z = x + y - 1 in
  z * (z+1) / 2 - x - 1

let next n = n * coeff mod div

let p1 l =
  let y, x = pos l in
  let r = ref init in
  for _ = 1 to pos2int x y do
    r := next !r
  done;
  string_of_int !r

let p2 _ = "END 2015!"
