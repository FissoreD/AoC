type info = {reindeer:string;speed:int; march_time:int;sleep:int}

let parse_line s =
  let s = String.split_on_char ' ' s in
  let reindeer = List.hd s in
  let speed = int_of_string (List.nth s 3) in
  let march_time = int_of_string (List.nth s 6) in
  let sleep = int_of_string (List.nth s 13) in
  {reindeer;speed;march_time;sleep}

let go time {speed;march_time;sleep;_} =
  let time_cycle = march_time + sleep in
  let total_km = speed * march_time in
  let d, m = (time / time_cycle, time mod time_cycle) in
  total_km * d + speed * min march_time m

let p1 l =
  let l = List.map parse_line l in
  List.map (go 2503) l |> List.fold_left max 0 |> string_of_int

let give_bonus l time bonus_acc =
  let max l b = match l with
    | [] -> [b]
    | (t,_) :: _ when t < fst b -> [b]
    | (t,_) :: _ when t = fst b -> b :: l
    | _ -> l in
  let bonus =
    List.map (fun x -> go time x,x) l
    |> List.fold_left max []
    |> List.map snd in
  let rec find_pos r pos = function
    | [] -> raise Not_found
    | hd :: _ when hd = r -> pos
    | _ :: tl -> find_pos r (pos+1) tl in
  let incr p = bonus_acc.(p) <- bonus_acc.(p) + 1 in
  List.iter (fun e -> find_pos e 0 l |> incr) bonus

let p2 l =
  let l = List.map parse_line l in
  let bonus = Array.make (List.length l) 0 in
  for i = 1 to 2503 do
    give_bonus l i bonus;
  done;
  Array.fold_left max 0 bonus |> string_of_int
