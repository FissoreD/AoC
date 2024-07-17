type ob = Output of int | Bot of int
type t = B of ob * ob | V of int

let parse_line l =
  let ios = int_of_string in
  let s2ob x n = if x = "bot" then Bot (ios n) else Output (ios n) in
  match Utils.find_all {|\(bot\)\|\(output\)\|\([0-9]+\)|} l with
  | [ "bot"; n; l; ln; h; hn ] -> (ios n, B (s2ob l ln, s2ob h hn))
  | [ n; "bot"; nb ] -> (ios n, V (ios nb))
  | _ -> Utils.error 2016 10 ""

let add_bots bots b v =
  match List.assoc_opt b bots with
  | None -> (b, ref [ v ]) :: bots
  | Some r ->
      r := List.sort compare [ v; List.hd !r ];
      bots

let init_values l =
  let rec aux bots actions = function
    | [] -> (actions, (bots, []))
    | (n, V b) :: tl -> aux (add_bots bots b n) actions tl
    | (n, B (c, d)) :: tl -> aux bots ((n, (c, d)) :: actions) tl
  in
  aux [] [] l

let add_value (bots, outputs) v = function
  | Output p -> (bots, add_bots outputs p v)
  | Bot b -> (add_bots bots b v, outputs)

exception STOP of int

let rec evolve is_p1 actions state = function
  | [] -> (state, false)
  | (b, ({ contents = [ x; y ] } as r)) :: _ ->
      r := [];
      if is_p1 && x = 17 && y = 61 then raise (STOP b);
      let c, d = List.assoc b actions in
      let state = add_value state x c in
      (add_value state y d, true)
  | _ :: tl -> evolve is_p1 actions state tl

let rec evolve_loop is_p1 actions state =
  let state, loop = evolve is_p1 actions state (fst state) in
  if not loop then snd state else evolve_loop is_p1 actions state

let p1 l =
  let actions, state = init_values (List.map parse_line l) in
  try
    let _ = evolve_loop true actions state in
    raise Not_found
  with STOP x -> string_of_int x

let p2 l =
  let actions, state = init_values (List.map parse_line l) in
  let output = evolve_loop false actions state in
  let get pos = !(List.assoc pos output) |> List.hd in
  get 0 * get 1 * get 2 |> string_of_int
