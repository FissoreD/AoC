include Int

let sqrt n = int_of_float (sqrt (float n))

let is_square n =
  let sq = sqrt n in
  sq * sq = n

let is_even n = n land 1 = 0
let is_odd n = n land 1 = 1
