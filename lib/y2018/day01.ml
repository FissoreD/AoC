let p1 l = List.map int_of_string l |> List.sum |> string_of_int

let p2 l =
  let all = List.map int_of_string l in
  let mem = Hashtbl.create 1024 in
  let rec find_twice acc l =
    if Hashtbl.mem mem acc then acc
    else
      match l with
      | [] -> find_twice acc all
      | x :: xs ->
          Hashtbl.replace mem acc ();
          find_twice (x + acc) xs
  in
  find_twice 0 all |> string_of_int
