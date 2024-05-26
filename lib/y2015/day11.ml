let rec next_password = function
  | [] -> []
  | 'z' :: tl -> 'a' :: next_password tl
  | x :: tl -> char_of_int (int_of_char x + 1) :: tl

let valid_password l =
  let is_next_chr a b = int_of_char a + 1 = int_of_char b in
  let rec consec = function
    | [] | [_] | [_;_] -> false
    | x::y::z::_ when is_next_chr z y && is_next_chr y x -> true
    | _ :: tl -> consec tl in
  let rec not_iol = function 
    | [] -> true
    | 'i' :: _ | 'o' :: _ | 'l' :: _ -> false
    | _ :: l -> not_iol l in
  let rec two_pair = function 
    | 2 -> fun _ -> true 
    | n -> function
      | [] | [_] -> false
      | x :: y :: l when x = y -> two_pair (n+1) l
      | _ :: l -> two_pair n l
  in
  consec l && not_iol l && two_pair 0 l

let rec next_valid_password l =
  let l = next_password l in
  if valid_password l then l else next_valid_password l

let print_psw l =
  let l = List.rev l in 
  List.fold_left (Printf.sprintf "%s%c") "" l

let parse_input l = 
  let hd = List.hd l in
  String.(List.init (length hd) (get hd)) |> List.rev

let p1 l =
  parse_input l |> next_valid_password |> print_psw |> print_string

let p2 l =
  parse_input l |> next_valid_password |> next_valid_password |> print_psw |> print_string
