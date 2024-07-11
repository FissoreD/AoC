let parse l = int_of_string (List.hd l)

let rec update ar len = function
  | 0 -> Dll.add_next_value len ar
  | n -> update (Dll.get_next ar) len (n - 1)

let rec repeat ar size a b =
  if a = b then ar else repeat (update ar b size) size a (b + 1)

let main size =
  let ar = repeat (Dll.singleton_circ 0) size (2017 + 1) 1 in
  string_of_int Dll.(find ar 2017 |> get_next |> get_value)

let p1 l = main (parse l)

let main step =
  let rec aux old pos = function
    | 50_000_001 -> old
    | n ->
        let pos = (pos + step) mod n in
        aux (if pos = 0 then n else old) (pos + 1) (n + 1)
  in
  aux 1 0 1 |> string_of_int

let p2 l = main (parse l)
