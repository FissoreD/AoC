let parse l = Utils.all_ints (List.hd l)

type tree = Node of { children : tree list; metadata : int list }
[@@deriving show]

let empty = Node { children = []; metadata = [] }

let rec split l acc = function
  | 0 -> (List.rev acc, l)
  | n -> split (List.tl l) (List.hd l :: acc) (n - 1)

let rec build_children n trees l =
  if n = 0 then (List.rev trees, l)
  else
    let tree, tl = build_tree l in
    build_children (n - 1) (tree :: trees) tl

and build_tree = function
  | [] -> (empty, [])
  | 0 :: md :: tl ->
      let metadata, tl = split tl [] md in
      (Node { children = []; metadata }, tl)
  | [ _ ] -> failwith "Error"
  | nb_child :: len_md :: tl ->
      let children, tl = build_children nb_child [] tl in
      let metadata, tl = split tl [] len_md in
      (Node { children; metadata }, tl)

let build_tree l = build_tree l |> fst

let p1 l =
  let rec sum_md (Node { children; metadata }) =
    List.sum metadata + List.sum (List.map sum_md children)
  in
  sum_md (build_tree (parse l)) |> string_of_int

let p2 l =
  let rec sum_md (Node { children; metadata }) =
    let get p = List.nth_opt children p |> Option.value ~default:empty in
    if children = [] then List.sum metadata
    else List.sum (List.map (fun pos -> sum_md (get (pos - 1))) metadata)
  in
  sum_md (build_tree (parse l)) |> string_of_int
