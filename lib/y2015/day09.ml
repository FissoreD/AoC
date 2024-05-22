module MapStr = Map.Make(String)

let parse_line l =
  match String.split_on_char ' ' l with
  | [a;"to";b;"=";n] -> a,b,int_of_string n
  | _ -> failwith l

let make_dist_map l = 
  let cities = ref [] in
  let add a = if not (List.mem a !cities) then cities := a :: !cities in
  let add_fun = (fun acc (a,b,n) -> 
    add a; add b; MapStr.add (b^a) n (MapStr.add (a^b) n acc)) in
  let a = List.fold_left add_fun MapStr.empty l in a, !cities

let rec rotate_list l = function 
  | 0 -> l
  | n -> match l with 
    | [] -> failwith "n should be smaller then len(l)"
    | x :: xs -> rotate_list (xs @ [x]) (n - 1)
  
let all_rotation l =
  let res = ref [] in
  for i = 0 to List.length l - 1 do
    res := rotate_list l i :: !res;
  done;
  !res

let l_f_hd_tl f = function
  | [] -> 0
  | hd :: tl -> f hd tl

let list_fold f = l_f_hd_tl (List.fold_left f)

let rec find_dist f dist_map l = 
  l_f_hd_tl (fun h1 l -> 
    let x = List.map 
      (l_f_hd_tl (fun h2 tl -> 
        MapStr.find (h1^h2) dist_map + find_dist f dist_map (h2 :: tl)))
      (all_rotation l) in
    list_fold f x) l

let find_dist f dist_map l =
  list_fold f (List.map (find_dist f dist_map) (all_rotation l))

let p1 l =  
  let l = List.map parse_line l in
  let dist_map, cities = make_dist_map l in
  find_dist min dist_map cities |> print_int

let p2 l =  
  let l = List.map parse_line l in
  let dist_map, cities = make_dist_map l in
  find_dist max dist_map cities |> print_int
