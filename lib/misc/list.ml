include List

let sum = List.fold_left ( + ) 0
let mul = List.fold_left ( * ) 1
let min = List.fold_left min max_int
let max = List.fold_left max min_int
let cat = List.fold_left ( ^ ) ""
