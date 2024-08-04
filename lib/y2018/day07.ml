let parse l =
  let x = String.split_on_char ' ' l in
  (List.nth x 1, List.nth x 7)

let parse = List.map parse
let get d a = Hashtbl.find_opt d a |> Option.value ~default:[]

let build_graph l =
  let succ = Hashtbl.create 1024 in
  let nodes = ref [] in
  let add (b, a) = Hashtbl.replace succ a (b :: get succ a) in
  List.iter add l;
  List.iter (fun (a, b) -> nodes := a :: b :: !nodes) l;
  (List.no_dup !nodes, succ)

let all_in l expl = List.for_all (fun e -> List.mem e expl) l
let no_pred g expl x = (not (Hashtbl.mem g x)) || all_in (Hashtbl.find g x) expl

let next g expl to_expl =
  List.filter (no_pred g expl) to_expl |> List.sort compare

let filter_not_in l1 l2 = List.filter (fun e -> not (List.mem e l2)) l1

let topo_sort (nodes, g) =
  let rec topo_sort explored to_explore =
    if to_explore = [] then ""
    else
      let x = next g explored to_explore |> List.hd in
      x ^ topo_sort (x :: explored) (filter_not_in to_explore [ x ])
  in
  topo_sort [] nodes

let p1 l = topo_sort (build_graph (parse l))
let step_time e = (60 + int_of_char e.[0] - int_of_char 'A' + 1, e)
let reduce_working = List.map (fun (t, e) -> (t - 1, e))

let topo_sort (nodes, g) =
  let rec topo_sort time workers explored working to_explore =
    let working = reduce_working working in
    if to_explore = [] then time + List.max (List.map fst working)
    else
      let fin, working = List.partition (fun (t, _) -> t = 0) working in
      let workers = workers + List.length fin in
      let explored = List.map snd fin @ explored in
      let next = next g explored to_explore in
      let len = min workers (List.length next) in
      let doing = List.sub next len in
      topo_sort (time + 1) (workers - len) explored
        (List.map step_time doing @ working)
        (filter_not_in to_explore doing)
  in
  topo_sort 0 5 [] [] nodes

let p2 l = topo_sort (build_graph (parse l)) |> string_of_int
