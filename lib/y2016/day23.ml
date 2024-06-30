type reg_val = Register of int | Value of int

type instr =
  | Cpy of reg_val * reg_val
  | Inc of int
  | Dec of int
  | Jnz of reg_val * reg_val
  | Tgl of int

let r2i = function "a" -> 0 | "b" -> 1 | "c" -> 2 | _ -> 3

let parse_row r =
  let s2rv r = try Value (int_of_string r) with _ -> Register (r2i r) in
  match String.split_on_char ' ' r with
  | [ "cpy"; n; reg ] -> Cpy (s2rv n, s2rv reg)
  | [ "jnz"; reg; n ] -> Jnz (s2rv reg, s2rv n)
  | [ "inc"; reg ] -> Inc (r2i reg)
  | [ "dec"; reg ] -> Dec (r2i reg)
  | [ "tgl"; c ] -> Tgl (r2i c)
  | _ -> failwith (Printf.sprintf "y2016/d23 invalid entry: %s" r)

let do_instr instr mem pos =
  let update reg value skip =
    mem.(reg) <- value;
    pos := !pos + skip
  in
  let getr = function Register r -> mem.(r) | Value i -> i in
  let geti skip = instr.(!pos + skip) in
  let toggle p =
    if !pos + mem.(p) >= Array.length instr then ()
    else
      instr.(!pos + mem.(p)) <-
        (match geti mem.(p) with
        | Dec i | Tgl i -> Inc i
        | Inc i -> Dec i
        | Jnz (a, b) -> Cpy (a, b)
        | Cpy (a, b) -> Jnz (a, b))
  in
  function
  | Inc r -> update r (mem.(r) + 1) 1
  | Dec r -> update r (mem.(r) - 1) 1
  | Cpy (_, Value _) -> incr pos
  | Cpy (Register n, Register r) -> update r mem.(n) 1
  | Cpy (Value v, Register r) -> update r v 1
  | Jnz (n, _) when getr n = 0 -> incr pos
  | Jnz (Register r, Value -2) ->
      (* Here fast add *)
      (match geti (if geti (-1) = Dec r then -2 else -1) with
      | Inc a -> update a (mem.(a) + mem.(r)) 1
      | _ -> failwith "Invalid inp");
      mem.(r) <- 0
  | Jnz (Register d, Value -5) ->
      (* Here fast mul: ad-hoc from my input *)
      (match (geti (-5), geti (-4)) with
      | Cpy (b, _), Inc a -> update a (mem.(a) + (getr b * mem.(d))) 1
      | _ -> pos := !pos - 5);
      mem.(d) <- 0
  | Jnz (Value _, s) | Jnz (Register _, s) -> pos := !pos + getr s
  | Tgl r ->
      toggle r;
      incr pos

let run_prog mem instr =
  let pos = ref 0 in
  let len = Array.length instr in
  while !pos >= 0 && !pos < len do
    do_instr instr mem pos instr.(!pos)
  done;
  mem.(0)

let main ~f l =
  let instr = List.map parse_row l |> Array.of_list in
  run_prog (Array.init 4 f) instr |> string_of_int

let p1 = main ~f:(function 0 -> 7 | _ -> 0)
let p2 = main ~f:(function 0 -> 12 | _ -> 0)
