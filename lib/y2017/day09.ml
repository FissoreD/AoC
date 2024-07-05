let rec count_par lvl = function
  | [] -> 0
  | '{' :: tl -> lvl + count_par (lvl + 1) tl
  | '}' :: tl -> count_par (lvl - 1) tl
  | _ :: tl -> count_par lvl tl

let split_comments s =
  let rec aux in_comment comm_char s l =
    if l = [] then (comm_char, List.rev s)
    else
      let hd, tl = List.(hd l, tl l) in
      if hd = '!' then aux in_comment comm_char s (List.tl tl) (* skip char *)
      else if in_comment then
        if hd = '>' then aux false comm_char s tl (* end comm *)
        else aux in_comment (comm_char + 1) s tl (* comm_char += 1 *)
      else if hd = '<' then aux true comm_char s tl (* enter comm *)
      else if hd = '}' then aux false comm_char (hd :: s) tl
      else aux false comm_char (hd :: s) tl
  in
  aux false 0 [] (String.to_list (List.hd s))

let p1 l = split_comments l |> snd |> count_par 1 |> string_of_int
let p2 l = split_comments l |> fst |> string_of_int
