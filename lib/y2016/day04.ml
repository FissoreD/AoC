let parse_line l =
  let open Str in
  search_forward (regexp {|\(.*\)-\([0-9]+\)\[\(.*\)\]|}) l 0 |> ignore;
  let get p = matched_group p l in
  let a, b, c = (get 1, int_of_string @@ get 2, get 3) in
  (split (regexp "") a, b, split (regexp "") c)

let count_max l =
  let module SS = Set.Make (String) in
  let s = SS.of_list l |> SS.remove "-" |> SS.elements in
  let cnt = List.map (fun x -> (x, Utils.count x l)) s in
  let compare (x1, x2) (y1, y2) =
    let l = compare x2 y2 * -1 in
    if l = 0 then String.compare x1 y1 else l
  in
  List.map fst (List.sort compare cnt)

let rec get_val n = function
  | _, [] -> n
  | x :: xs, y :: ys when x = y -> get_val n (xs, ys)
  | _, _ -> 0

let rec count = function
  | [] -> 0
  | (a, b, c) :: tl -> get_val b (count_max a, c) + count tl

let p1 l = List.map parse_line l |> count |> string_of_int
let parse_line2 (a, b, _) = (List.cat a, b)

let next l =
  let open String in
  let a = int_of_char 'a' in
  let next_char = function
    | '-' -> '-'
    | c -> char_of_int (((int_of_char c - a + 1) mod 26) + a)
  in
  init (length l) (fun i -> next_char l.[i])

let rec rotate (l, x) = function 0 -> (l, x) | n -> rotate (next l, x) (n - 1)

let p2 l =
  let open List in
  map parse_line l |> map parse_line2
  |> map (fun x -> rotate x (snd x))
  |> find (fun (x, _) -> Str.(string_match (regexp ".*north.*")) x 0)
  |> snd |> string_of_int
