type av = {hp:int; dmg:int; mana:int; arm:int}
type spell = Missile | Drain | Shield | Poison | Recharge

let parse_file l =
  match List.(map Utils.all_ints l |> map hd) with
  | [hp;dmg] -> {hp;dmg;mana=0;arm=0}
  | _ -> failwith "Invalid arg"

let is_dead p = p.hp <= 0
let remove_hp pv p = {p with hp = p.hp - pv}

let apply_spell (p,m) s = match snd s with
  | Missile -> p, remove_hp 4 m
  | Drain -> remove_hp ~-2 p, remove_hp 2 m
  | Shield -> {p with arm=p.arm + 7}, m
  | Poison -> p, remove_hp 3 m
  | Recharge -> {p with mana=p.mana+101}, m

(* Cost, Time, Dmg, Heal, Mana, Armor  *)
let spells =
  [Missile,53,1; Drain,73,1; Shield,113,6; Poison,173,6; Recharge,229,5]

let can_do_spell (p,l) (s,c,_) =
  c <= p.mana && List.for_all (fun (_,s') -> s <> s') l

let rec update_spells = function
  | [] -> []
  | (1, _) :: tl -> update_spells tl
  | (x, s) :: tl -> (x-1, s) :: update_spells tl

let cast_spell p c = {p with mana = p.mana - c}

(* mv is min mana with victory, c is current mana spent *)
let rec run ~hard mv isP c (p,m,l) =
  let p = if isP then hard p else p in
  if mv < c || is_dead p then mv else
  let p,m = List.fold_left apply_spell (p,m) l in
  let l = update_spells l in
  if is_dead m then c
  else if not isP then
      let p = remove_hp (max 1 (m.dmg - p.arm)) p in
    run ~hard mv (not isP) c (p,m,l)
  else
    let p = {p with arm = 0} in
    let l1 = List.filter (can_do_spell (p,l)) spells in
    if l1 = [] then mv else
    List.fold_left (fun mv (s,c',t) ->
      let p = cast_spell p c' in
      run ~hard mv (not isP) (c+c') (p,m,((t,s)::l))) mv l1


let p1 l =
  let m = parse_file l in
  let p = {hp=50;dmg=0;mana=500;arm=0} in
  run ~hard:Fun.id max_int true 0 (p,m,[]) |> string_of_int

let p2 l =
  let m = parse_file l in
  let p = {hp=50;dmg=0;mana=500;arm=0} in
  run ~hard:(remove_hp 1) max_int true 0 (p,m,[]) |> string_of_int
