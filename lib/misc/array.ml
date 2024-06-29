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
