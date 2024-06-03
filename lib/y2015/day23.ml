type reg = A | B

type instr = 
  | Hlf of reg | Tpl of reg | Inc of reg
  | Jmp of int | Jie of reg * int | Jio of reg * int

let to_reg = function "a" -> A | _ -> B  
let parse_line l =
  let l = Str.(split (regexp",? ") l) in
  let i = int_of_string in
  match l with
  | "hlf" :: [r] -> Hlf (to_reg r)
  | "inc" :: [r] -> Inc (to_reg r)
  | "tpl" :: [r] -> Tpl (to_reg r)
  | "jio" :: r :: [n] -> Jio ((to_reg r),(i n))
  | "jie" :: r :: [n] -> Jie ((to_reg r),(i n))
  | "jmp" :: [n] -> Jmp (i n)
  | _ -> invalid_arg "y15/d23: error"

let get a b = function A -> a | B -> b

let apply x i = x := i !x

let do_instr a b j = function
  | Hlf s -> apply (get a b s) (fun x -> x / 2); incr j
  | Inc s -> apply (get a b s) ((+) 1); incr j
  | Tpl s -> apply (get a b s) (Int.mul 3); incr j
  | Jmp s -> apply j ((+) s)
  | Jie (s,n) when !(get a b s) mod 2 = 0 -> j := !j + n
  | Jio (s,n) when !(get a b s) = 1 -> j := !j + n
  | _ -> incr j

let run inst a =
  let a, b, pos = ref a, ref 0, ref 0 in 
  while 0 <= !pos && !pos < Array.length inst do
    do_instr a b pos inst.(!pos);
  done; 
  !b

let p1 l =
  let l = List.map parse_line l |> Array.of_list in
  run l 0 |> print_int

let p2 l = 
  let l = List.map parse_line l |> Array.of_list in
  run l 1 |> print_int

