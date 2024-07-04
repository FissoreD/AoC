let build_tower l =
  let l = List.map (Utils.find_all {|\([a-z]\|[0-9]\)+|}) l in
  let childrens = Hashtbl.create 1024 in
  let fathers = Hashtbl.create 1024 in
  let add_line l =
    let father, nb, children = List.(hd l, hd (tl l), tl (tl l)) in
    Hashtbl.add childrens father (int_of_string nb, children);
    List.iter (fun x -> Hashtbl.add fathers x father) children
  in
  List.iter add_line l;
  (fathers, childrens)

let p1 l =
  let father, children = build_tower l in
  Seq.find (fun x -> not (Hashtbl.mem father x)) (Hashtbl.to_seq_keys children)
  |> Option.get

exception Stop of int

let correct_val expected given value =
  let diff = abs (expected - given) in
  raise @@ Stop (value + if expected > given then diff else -diff)

let rec find_err (w1, v) = function
  | [] -> w1
  | (w2, _) :: (w3, _) :: _ when w2 = w3 && w1 <> w2 -> correct_val w2 w1 v
  | (w2, v) :: (w3, _) :: _ when w1 = w2 && w2 <> w3 -> correct_val w1 w2 v
  | (w2, _) :: (w3, v) :: _ when w1 = w3 && w2 <> w3 -> correct_val w1 w3 v
  | (w2, _) :: tl -> w2 + find_err (w1, v) tl

let rec weights children father =
  let w, sons = Hashtbl.find children father in
  if sons = [] then (w, w)
  else
    let sons_weights = List.map (weights children) sons in
    (w + find_err (List.hd sons_weights) (List.tl sons_weights), w)

let p2 l =
  let (_, children), father = (build_tower l, p1 l) in
  try weights children father |> snd |> string_of_int (* should never be here *)
  with Stop s -> string_of_int s
