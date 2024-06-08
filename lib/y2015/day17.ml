let parse_file l = List.map int_of_string l

let rec fill max curr l = 
  if curr > max then 0 else 
  if curr = max then 1 else 
  match l with
  | [] -> 0
  | hd :: tl -> fill max (curr + hd) tl + fill max curr tl 

let p1 l = 
  parse_file l |> fill 150 0 |> string_of_int

let rec min_cont max (min_cnt_nb,min_tot_found as min_) (curr_cnt_nb,curr_capacity) l = 
  if min_cnt_nb < curr_cnt_nb || curr_capacity > max then min_
  else if max = curr_capacity then 
    if curr_cnt_nb = min_cnt_nb then curr_cnt_nb, min_tot_found + 1
    else curr_cnt_nb, 1
  else match l with
  | [] -> min_
  | hd :: tl ->
    let min_ = min_cont max min_ (curr_cnt_nb + 1, hd + curr_capacity) tl in
    min_cont max min_ (curr_cnt_nb, curr_capacity) tl

let p2 l =   
  parse_file l |> min_cont 150 (max_int, 0) (0, 0) |> snd |> string_of_int
