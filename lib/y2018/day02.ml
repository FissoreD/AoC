let count str c =
  let res = ref 0 in
  for i = 0 to String.length str - 1 do
    if str.[i] = c then incr res
  done;
  !res

let isN n str =
  let exception STOP in
  try
    for i = int_of_char 'a' to int_of_char 'z' do
      if count str (char_of_int i) = n then raise STOP
    done;
    false
  with STOP -> true

let p1 l =
  let is2 = List.filter (isN 2) l |> List.length in
  let is3 = List.filter (isN 3) l |> List.length in
  is2 * is3 |> string_of_int

let distance w1 w2 =
  let dist = ref 0 in
  let res = ref "" in
  for i = 0 to String.length w1 - 1 do
    if w1.[i] <> w2.[i] then incr dist
    else res := Printf.sprintf "%s%c" !res w1.[i]
  done;
  (!dist, !res)

let p2 l =
  let rec min_dist w1 = function
    | [] -> Utils.error 2018 2 ""
    | w2 :: tl as l -> (
        let d = List.map (distance w1) l in
        match List.find_opt (fun x -> fst x = 1) d with
        | None -> min_dist w2 tl
        | Some (_, x) -> x)
  in
  min_dist (List.hd l) (List.tl l)
