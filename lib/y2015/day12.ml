let rec find_int_sum : Yojson.Basic.t -> int = function
  | `Assoc s -> List.fold_left (fun a (_,b) -> a + find_int_sum b) 0 s
  | `Null | `Bool _ | `Float _ | `String _ -> 0
  | `Int i -> i
  | `List l -> List.map find_int_sum l |> List.fold_left (+) 0

let p1 s = 
  let json = Yojson.Basic.from_string (List.hd s) in
  find_int_sum json |> print_int

let rec find_int_sum_no_red : Yojson.Basic.t -> int = function
  | `Assoc s when List.for_all (function (_, `String "red") -> false | _ -> true) s -> 
      List.fold_left (fun a (_,b) -> a + find_int_sum_no_red b) 0 s
  | `Null | `Bool _ | `Float _ | `String _ | `Assoc _ -> 0
  | `Int i -> i
  | `List l -> List.map find_int_sum_no_red l |> List.fold_left (+) 0

let p2 s = 
  let json = Yojson.Basic.from_string (List.hd s) in
  find_int_sum_no_red json |> print_int

