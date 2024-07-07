let parse l =
  let l = List.map Utils.all_ints l in
  let tbl = Hashtbl.create 1024 in
  let add k v =
    if Hashtbl.mem tbl k then Hashtbl.replace tbl k (v :: Hashtbl.find tbl k)
    else Hashtbl.add tbl k [ v ]
  in
  List.iter
    (fun x ->
      let hd, tl = (List.hd x, List.tl x) in
      Hashtbl.add tbl hd tl;
      List.iter (fun x -> add x hd) tl)
    l;
  tbl

let rec group graph explored current =
  if (not (Hashtbl.mem explored current)) && Hashtbl.mem graph current then (
    Hashtbl.replace explored current ();
    Hashtbl.find graph current |> List.iter (group graph explored))

let p1 l =
  let explored = Hashtbl.create 1024 in
  group (parse l) explored 0;
  Seq.length (Hashtbl.to_seq_keys explored) |> string_of_int

let p2 l =
  let graph = parse l in
  let to_exp, exp = (ref [], Hashtbl.create 1024) in
  let add x = if not (List.mem x !to_exp) then to_exp := x :: !to_exp in
  let rm x = to_exp := List.filter (( <> ) x) !to_exp in
  Hashtbl.to_seq_keys graph |> Seq.iter add;
  Hashtbl.to_seq_values graph |> Seq.iter (fun x -> List.iter add x);
  let group_nb = ref 0 in
  while !to_exp <> [] do
    Hashtbl.clear exp;
    group graph exp (List.hd !to_exp);
    Hashtbl.to_seq_keys exp |> Seq.iter rm;
    incr group_nb
  done;
  !group_nb |> string_of_int
