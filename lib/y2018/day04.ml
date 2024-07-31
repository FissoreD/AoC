type action = S | W | Guard of int
type t = (int * int * int) * (int * int) [@@deriving show]

let parse l =
  let l = List.sort String.compare l in
  let parse_tuple a b c d e _ s =
    let in_s prefix = String.starts_with ~prefix s in
    let time = ((a, b, c), (d, e)) in
    let int () = List.hd (Utils.all_ints s) in
    (time, if in_s "a" then S else if in_s "u" then W else Guard (int ()))
  in
  let parse_line l = Scanf.sscanf l "[%d-%d-%d %d:%d] %s %s" parse_tuple in
  let parsed = List.map parse_line l in
  let rec make_l guard acc = function
    | [] -> [ (guard, List.sort compare acc) ]
    | (e, Guard i) :: xs -> (guard, List.sort compare acc) :: make_l i [ e ] xs
    | (e, _) :: xs -> make_l guard (e :: acc) xs
  in
  make_l (-1) [] parsed |> List.tl

let is_leap_year l = if l mod 100 = 0 then l mod 400 = 0 else l mod 4 = 0

let to_min ((y, m, d), (h, mi)) =
  let feb = if is_leap_year y then 29 else 28 in
  let l = [ 31; feb; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 ] in
  let y2d = y * List.sum l in
  ((((y2d + List.sum (List.sub l (m - 1)) + d) * 24) + h) * 60) + mi

let add_min ar a b =
  for i = to_min a to to_min b - 1 do
    ar.(i mod 60) <- ar.(i mod 60) + 1
  done

let get_max_min ar = Array.index_of ar (Array.max ar)

let rec add_sleep ar d = function
  | [] -> ()
  | d' :: tl when d <> None ->
      add_min ar (Option.get d) d';
      add_sleep ar None tl
  | d :: tl -> add_sleep ar (Some d) tl

let main l f =
  (* each guard is associated to an array of minute of sleep *)
  let tbl = parse l in
  let guards = List.map fst tbl |> List.no_dup in
  let build_ar () = Array.make 60 0 in
  let ars = Hashtbl.create (List.length guards) in
  List.iter (fun e -> Hashtbl.add ars e (build_ar ())) guards;
  List.iter (fun (e, l) -> add_sleep (Hashtbl.find ars e) None (List.tl l)) tbl;
  let keep_max a b (c, d) = if f b >= f d then (a, b) else (c, d) in
  let guardid, ar = Hashtbl.fold keep_max ars (0, [||]) in
  guardid * get_max_min ar |> string_of_int

let p1 l = main l Array.sum
let p2 l = main l Array.max
