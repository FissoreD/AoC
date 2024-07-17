let parse l = List.hd l |> int_of_string
let rec ones = function 0 -> 0 | n -> (n land 1) + ones (n lsr 1)

module M (S : sig
  val fn : int
end) =
struct
  let explored = ref []
  let add_explored p = explored := p :: !explored

  let is_free (x, y) =
    let f x y = (x * x) + (3 * x) + (2 * x * y) + y + (y * y) in
    f x y + S.fn |> ones |> fun x -> x mod 2 = 0

  let neigh = Pos.neigh4
  let valid p = Pos.valid00 p && is_free p && not (List.mem p !explored)
end

let bfs is_p1 fn =
  let module X = M (struct
    let fn = fn
  end) in
  let module Pos = Pos.Make (X) in
  let dist = ref 0 in
  let pos = ref [ (1, 1) ] in
  let while_cond =
    if is_p1 then fun () -> not (List.mem (31, 39) !pos)
    else fun () -> !dist <= 50
  in
  while while_cond () do
    List.iter X.add_explored !pos;
    (pos := List.(no_dup (map Pos.get_neigh !pos |> flatten)));
    incr dist;
    if !pos = [] then Utils.error 2016 3 "bfs error"
  done;
  if is_p1 then !dist else List.length !X.explored

let p1 n = bfs true (parse n) |> string_of_int
let p2 n = bfs false (parse n) |> string_of_int
