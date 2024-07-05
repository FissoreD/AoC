let to2 = function [ a; b ] -> (a, b) | _ -> failwith "Tuple2: invalid entry"

let to3 = function
  | [ a; b; c ] -> (a, b, c)
  | _ -> failwith "Tuple3: invalid entry"

let to4 = function
  | [ a; b; c; d ] -> (a, b, c, d)
  | _ -> failwith "Tuple4: invalid entry"

let to5 = function
  | [ a; b; c; d; e ] -> (a, b, c, d, e)
  | _ -> failwith "Tuple5: invalid entry"

let to6 = function
  | [ a; b; c; d; e; f ] -> (a, b, c, d, e, f)
  | _ -> failwith "Tuple6: invalid entry"

let to7 = function
  | [ a; b; c; d; e; f; g ] -> (a, b, c, d, e, f, g)
  | _ -> failwith "Tuple6: invalid entry"
