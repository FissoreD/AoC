let parse l =
  let a, b, c, d, e = Utils.all_ints l |> Tuple.to5 in
  (a, ((b, c), (d, e)))

let parse = List.map parse

let give_overlap l =
  let explored = Hashtbl.create 1024 in
  let overlap = Hashtbl.create 1024 in
  let add (_, ((x, y), (w, h))) =
    for x = x to w + x - 1 do
      for y = y to y + h - 1 do
        if Hashtbl.mem explored (x, y) then Hashtbl.replace overlap (x, y) ();
        Hashtbl.replace explored (x, y) ()
      done
    done
  in
  List.iter add l;
  overlap

let p1 l = give_overlap (parse l) |> Hashtbl.length |> string_of_int

let p2 l =
  let l = parse l in
  let tbl = give_overlap l in
  let exception Stop of int in
  let is_free (id, ((x, y), (w, h))) =
    try
      for x = x to x + w - 1 do
        for y = y to y + h - 1 do
          if Hashtbl.mem tbl (x, y) then raise Not_found
        done
      done;
      raise (Stop id)
    with Not_found -> ()
  in
  (try snd (List.iter is_free l, -1) with Stop x -> x) |> string_of_int
