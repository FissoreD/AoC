include Int

let sqrt n = int_of_float (sqrt (float n))

let is_square n =
  let sq = sqrt n in
  sq * sq = n

let is_even n = n land 1 = 0
let is_odd n = n land 1 = 1

let is_prime i =
  if i = 2 then true
  else if i mod 2 = 0 then false
  else
    let exception STOP in
    let x = ref 3 in
    try
      let stop = sqrt i in
      while !x <= stop do
        if i mod !x = 0 then raise STOP;
        x := !x + 2
      done;
      true
    with STOP -> false
