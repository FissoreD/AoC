let int_triple f s = 
  match String.split_on_char 'x' s |> List.map int_of_string with
  | [a;b;c] -> f a b c
  | _ -> invalid_arg "invalid int triple" 

let p1 s =
  let f a b c = 
    let l = [a*b;b*c;a*c] in
      2 * (List.fold_left (+) 0 l) + (List.fold_left min max_int l) in
  List.map (int_triple f) s |> List.fold_left (+) 0 |> print_int

let p2 s =   
  let f a b c =
    let x = List.sort Int.compare [a;b;c] in
    a * b * c + (List.hd x + List.hd (List.tl x)) * 2 in
  List.map (int_triple f) s |> List.fold_left (+) 0 |> print_int


