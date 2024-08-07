let get_first_line = function
  | [ s ] -> s
  | _ -> invalid_arg "day01: invalid arg"

let p1 s =
  let s = get_first_line s in
  let l = String.length s in
  let r = ref 0 in
  for i = 0 to l - 1 do
    match s.[i] with
    | '(' -> incr r
    | ')' -> decr r
    | _ -> Utils.error 2015 1 ""
  done;
  string_of_int !r

let p2 s =
  let s = get_first_line s in
  let r = ref 0 in
  let i = ref 0 in
  while !r >= 0 do
    (match s.[!i] with
    | '(' -> incr r
    | ')' -> decr r
    | _ -> Utils.error 2015 1 "");
    incr i
  done;
  string_of_int !i
