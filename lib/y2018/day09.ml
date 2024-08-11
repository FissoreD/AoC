let parse l = Utils.all_ints (List.hd l) |> Tuple.to2

type t = { mutable next : t; value : int; mutable prev : t }

let create value =
  let rec res = { next = res; value; prev = res } in
  res

let add_after value cnt =
  let new_cell = { next = cnt.next; value; prev = cnt } in
  cnt.next.prev <- new_cell;
  cnt.next <- new_cell;
  new_cell

let remove_cell cnt =
  cnt.prev.next <- cnt.next;
  cnt.next.prev <- cnt.prev;
  cnt.next

let go_back_7 cnt =
  let rec aux cnt = function 7 -> cnt | n -> aux cnt.prev (n + 1) in
  aux cnt 0

let main mult cnt =
  let players, max_points = cnt in
  let max_points = max_points * mult in
  let stats = Array.make players 0 in
  let rec aux next23mul current = function
    | n when n = max_points + 1 -> Array.fold_left max 0 stats
    | n when n = next23mul ->
        let back = go_back_7 current in
        let player_pos = (n - 1) mod players in
        stats.(player_pos) <- stats.(player_pos) + n + back.value;
        aux (next23mul + 23) (remove_cell back) (n + 1)
    | n -> aux next23mul (add_after n current.next) (n + 1)
  in
  aux 23 (create 0) 1 |> string_of_int

let p1 l = main 1 (parse l)
let p2 l = main 100 (parse l)
