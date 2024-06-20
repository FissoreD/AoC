let rec find_int_sum : Yojson.Basic.t -> int = function
  | `Assoc s -> List.map snd s |> List.map find_int_sum |> List.sum
  | `Null | `Bool _ | `Float _ | `String _ -> 0
  | `Int i -> i
  | `List l -> List.map find_int_sum l |> List.sum

let p1 s =
  let json = Yojson.Basic.from_string (List.hd s) in
  find_int_sum json |> string_of_int

let rec find_int_sum_no_red : Yojson.Basic.t -> int = function
  | `Assoc s
    when List.for_all (function _, `String "red" -> false | _ -> true) s ->
      List.map snd s |> List.map find_int_sum_no_red |> List.sum
  | `Null | `Bool _ | `Float _ | `String _ | `Assoc _ -> 0
  | `Int i -> i
  | `List l -> List.map find_int_sum_no_red l |> List.sum

let p2 s =
  let json = Yojson.Basic.from_string (List.hd s) in
  find_int_sum_no_red json |> string_of_int
