let str2op = function
  | "!=" -> ( <> )
  | "==" -> ( = )
  | ">=" -> ( >= )
  | ">" -> ( > )
  | "<=" -> ( <= )
  | "<" -> ( < )
  | x -> Utils.error 2017 8 x

let parse l =
  let ios = int_of_string in
  let x, op1, v1, _, y, op2, v2 = Tuple.to7 (String.split_on_char ' ' l) in
  let v1, v2 = (ios v1, ios v2) in
  let a1 = if op1 = "inc" then ( + ) v1 else fun x -> x - v1 in
  let a2 = str2op op2 in
  (x, a1, y, fun x -> a2 x v2)

let main l =
  let mem = Hashtbl.create 1024 in
  let get x = Hashtbl.find_opt mem x |> Option.value ~default:0 in
  let max_ = ref min_int in
  let exec (x, a1, y, a2) =
    if a2 (get y) then (
      max_ := max !max_ (a1 (get x));
      Hashtbl.replace mem x (a1 (get x)))
  in
  List.iter exec (List.map parse l);
  (Hashtbl.to_seq_values mem |> List.of_seq |> List.max, !max_)

let p1 l = main l |> fst |> string_of_int
let p2 l = main l |> snd |> string_of_int
