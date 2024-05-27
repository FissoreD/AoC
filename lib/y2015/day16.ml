let hints = 
  ["children", ((=), 3);
    "cats", ((=), 7);
    "samoyeds", ((=), 2);
    "pomeranians", ((=), 3);
    "akitas", ((=), 0);
    "vizslas", ((=), 0);
    "goldfish", ((=), 5);
    "trees", ((=), 3);
    "cars", ((=), 2);
    "perfumes", ((=), 1);]

let valid_sue hints prop =
  let valid (key, (rel, value)) =
    match List.assoc_opt key prop with
    | None -> true
    | Some a -> rel a value in
  List.for_all valid hints

let parse_row l =
  let rex = Str.regexp {|Sue \([0-9]+\): \([a-z]+\): \([0-9]+\), \([a-z]+\): \([0-9]+\), \([a-z]+\): \([0-9]+\)|} in
  let repl = Str.global_replace rex {|\1-\2-\3-\4-\5-\6-\7|} l in
  let l = String.split_on_char '-' repl in
  let nb, l = List.hd l, List.tl l in
  let rec aux = function 
    | [] -> []
    | [_] -> raise (Invalid_argument "y15/d16")
    | a::b::l -> (a, int_of_string b) :: aux l in
  nb, aux l

let get hints l = List.map parse_row l |> List.find (fun (_, y) -> valid_sue hints y) |> fst

let p1 l = get hints l |> print_string

let p2 l =
  let new_rel = ["cats", (>);"trees", (>); "goldfish", (<)] in
  let replace hints (key, rel) =
    let _,nb = List.assoc key hints in 
    (key, (rel, nb)) :: List.remove_assoc key hints in
  let hints = List.fold_left replace hints new_rel in
  get hints l |> print_string
