let parse_line l : (int * int * int) = 
  let to_triple = function | [a;b;c] -> (a,b,c) | _ -> failwith "y16/03 Invalid arg" in
  let l = Str.(split (regexp " +")) l in
  let l = List.map int_of_string l in
  to_triple l 

let valid_triangle (a, b, c) = a + b > c && b + c > a && a + c > b

let p1 l =
  let open List in
  map parse_line l |> filter valid_triangle |> length |> string_of_int

let rotate_triples (x1, x2, x3) (y1,y2,y3) (z1,z2,z3) =
  let a = if valid_triangle (x1,y1,z1) then 1 else 0 in
  let b = if valid_triangle (x2,y2,z2) then 1 else 0 in
  let c = if valid_triangle (x3,y3,z3) then 1 else 0 in 
  a + b + c

let rec rotate_triangles = function 
  | [] -> 0
  | a :: b :: c :: tl -> rotate_triples a b c + rotate_triangles tl
  | _ -> failwith "y16/d03 Invalid arg"

let p2 l = 
  let open List in
  map parse_line l |> rotate_triangles |> string_of_int
