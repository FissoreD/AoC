type t = S of string | M of int * int

let int_len i = String.length (string_of_int i)

let parse_line l =
  let l = Utils.find_all (Str.regexp {|[0-9]+\|[A-Z]|}) l in
  let rec aux = function
    | [] -> []
    | a :: b :: tl when Utils.is_int a ->
        M (int_of_string a, int_of_string b) :: aux tl
    | x :: xs -> S x :: aux xs
  in aux l

let crop =
  let rec aux acc n l =
    if n = 0 then (List.rev acc, l)
    else match l with
      | [] -> failwith "y2016/d09 invalid input"
      | M (a, b) :: l ->
          let len = int_len a + int_len b + 3 in
          aux (M (a, b) :: acc) (n - len) l
      | s :: l -> aux (s :: acc) (n - 1) l
  in aux []

let decompress do_pref =
  let rec aux acc = function
    | [] -> acc
    | S _ :: l -> aux (acc + 1) l
    | M (a, b) :: l ->
        let pref, suff = crop a l in
        let sub_len = if do_pref then aux 0 pref else a in
        aux (acc + (sub_len * b)) suff
  in aux 0

let p1 l = decompress false (parse_line (List.hd l)) |> string_of_int

let p2 l = decompress true (parse_line (List.hd l)) |> string_of_int
