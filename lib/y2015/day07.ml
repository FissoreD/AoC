type int_str = Int of int | Str of string

type instr =
  | Affect of int_str
  | Unary of (int -> int) * int_str
  | Binary of (int -> int -> int) * int_str * int_str

module Mem = Map.Make (String)

let get m = function Int i -> i | Str s -> Mem.find s m

let run_line mem s = function
  | Affect n -> Mem.add s (get mem n) mem
  | Unary (f, s1) -> Mem.add s (f (get mem s1)) mem
  | Binary (f, s1, s2) -> Mem.add s (f (get mem s1) (get mem s2)) mem

let rec run_lines_aux mem = function
  | [] -> (mem, [])
  | (instr, ad) :: tl -> (
      try
        let mem = run_line mem ad instr in
        run_lines_aux mem tl
      with Not_found ->
        let mem, tl = run_lines_aux mem tl in
        (mem, (instr, ad) :: tl))

let rec run_lines mem l =
  let mem, l = run_lines_aux mem l in
  if l = [] then mem else run_lines mem l

let to_int_str s =
  match int_of_string_opt s with None -> Str s | Some s -> Int s

let parse_unary = function
  | "NOT" -> fun x -> -x - 1
  | a -> Utils.error 2015 7 ("Not implemented unary operator " ^ a)

let parse_binary = function
  | "AND" -> ( land )
  | "OR" -> ( lor )
  | "LSHIFT" -> ( lsl )
  | "RSHIFT" -> ( lsr )
  | a -> Utils.error 2015 7 ("Not implemented binary operator " ^ a)

let parse_line l =
  match String.split_on_char ' ' l with
  | [ s; "->"; ad ] -> (Affect (to_int_str s), ad)
  | [ un; s; "->"; ad ] -> (Unary (parse_unary un, to_int_str s), ad)
  | [ s1; bin; s2; "->"; ad ] ->
      (Binary (parse_binary bin, to_int_str s1, to_int_str s2), ad)
  | _ -> Utils.error 2015 7 l

let run_lines l =
  let mem = Mem.empty in
  run_lines mem l |> Mem.find "a"

let p1 l = run_lines (List.map parse_line l) |> string_of_int

let p2 l =
  let l = List.map parse_line l in
  let val_a = run_lines l in
  let affect_b = function _, "b" -> (Affect (Int val_a), "b") | a -> a in
  List.map affect_b l |> run_lines |> string_of_int
