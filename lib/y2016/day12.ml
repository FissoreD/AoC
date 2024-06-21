type reg_val = Register of int | Value of int

type instr =
  | Cpy of reg_val * int
  | Inc of int
  | Dec of int
  | Jnz of reg_val * int

let r2i = function "a" -> 0 | "b" -> 1 | "c" -> 2 | _ -> 3

let parse_row r =
  let s2rv r = try Value (int_of_string r) with _ -> Register (r2i r) in
  match String.split_on_char ' ' r with
  | [ "cpy"; n; reg ] -> Cpy (s2rv n, r2i reg)
  | [ "jnz"; reg; n ] -> Jnz (s2rv reg, int_of_string n)
  | [ "inc"; reg ] -> Inc (r2i reg)
  | [ "dec"; reg ] -> Dec (r2i reg)
  | _ -> failwith (Printf.sprintf "y2016/d12 invalid entry: %s" r)

let do_instr mem pos =
  let update reg value skip =
    mem.(reg) <- value;
    pos := !pos + skip
  in
  function
  | Inc r -> update r (mem.(r) + 1) 1
  | Dec r -> update r (mem.(r) - 1) 1
  | Cpy (Register n, r) -> update r mem.(n) 1
  | Cpy (Value v, r) -> update r v 1
  | Jnz (Value 0, _) -> incr pos
  | Jnz (Value _, n) -> pos := !pos + n
  | Jnz (Register n, _) when mem.(n) = 0 -> incr pos
  | Jnz (Register _, s) -> pos := !pos + s

let run_prog mem instr =
  let pos = ref 0 in
  let len = Array.length instr in
  while !pos >= 0 && !pos < len do
    do_instr mem pos instr.(!pos)
  done;
  mem.(0)

let main ~f l =
  let instr = List.map parse_row l |> Array.of_list in
  let mem = Array.make 4 0 in
  f mem;
  run_prog mem instr |> string_of_int

let p1 = main ~f:ignore
let p2 = main ~f:(fun e -> e.(r2i "c") <- 1)
