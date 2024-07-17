type pt = { hp : int; dmg : int; arm : int }

(* 1 weapon, 0..1 armor, 0..2 rings (no reps) *)
let weapon = [ (8, 4, 0); (10, 5, 0); (25, 6, 0); (40, 7, 0); (74, 8, 0) ] (*5*)

let armor =
  [ (0, 0, 0); (13, 0, 1); (31, 0, 2); (53, 0, 3); (75, 0, 4); (102, 0, 5) ]

let rings =
  [
    (0, 0, 0);
    (25, 1, 0);
    (50, 2, 0);
    (100, 3, 0);
    (20, 0, 1);
    (40, 0, 2);
    (80, 0, 3);
  ]

let parse_file l =
  match List.(map Utils.all_ints l |> map hd) with
  | [ hp; dmg; arm ] -> { hp; dmg; arm }
  | _ -> Utils.error 2015 21 "Invalid arg"

let rec run_game isP p1 p2 =
  if p1.hp <= 0 then false
  else if p2.hp <= 0 then true
  else if isP then
    run_game (not isP) p1 { p2 with hp = p2.hp - p1.dmg + p2.arm }
  else run_game (not isP) { p1 with hp = p1.hp - p2.dmg + p1.arm } p2

let pp_pt s = Printf.sprintf "{%d,%d}" s.dmg s.arm
let pp_int_pt (p, pt) = Printf.sprintf "(%d, %s)" p (pp_pt pt)

let choose_items hp =
  let add (c2, pt) (c, d, a) =
    (c + c2, { hp; arm = pt.arm + a; dmg = pt.dmg + d })
  in
  let empty_st = (0, { hp; arm = 0; dmg = 0 }) in
  let next_gen f1 f2 l st hd tl = f1 (add st hd) l @ f2 st tl in
  let rec next_gen_l f1 l st = function
    | [] -> []
    | hd :: tl -> next_gen f1 (next_gen_l f1 l) (l tl) st hd tl
  in
  let next_ring_single st l = st :: List.map (add st) l in
  let next_ring_pair st = next_gen_l next_ring_single Fun.id st in
  let next_armor st = next_gen_l next_ring_pair (Fun.const rings) st in
  let next_weapon st = next_gen_l next_armor (Fun.const armor) st in
  next_weapon empty_st weapon |> List.no_dup

let rec find_cheap_victory cost monst = function
  | [] -> cost
  | (c1, pt) :: tl when c1 < cost && run_game true pt monst ->
      find_cheap_victory c1 monst tl
  | _ :: tl -> find_cheap_victory cost monst tl

let main f def l =
  let monst = parse_file l in
  let all_pt = choose_items 100 in
  f def monst all_pt |> string_of_int

let p1 = main find_cheap_victory max_int

let rec find_expensive_lose cost monst = function
  | [] -> cost
  | (c1, pt) :: tl when c1 > cost && not (run_game true pt monst) ->
      find_expensive_lose c1 monst tl
  | _ :: tl -> find_expensive_lose cost monst tl

let p2 = main find_expensive_lose min_int
