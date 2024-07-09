type t = X of (int * int) | P of (char * char) | S of int

let parse l =
  let parse s =
    let hd, ints = (s.[0], Utils.all_ints s) in
    if hd = 's' then S (List.hd ints)
    else if hd = 'x' then X (Tuple.to2 ints)
    else P (s.[1], s.[3])
  in
  List.map parse (String.split_on_char ',' (List.hd l))

let move mem ar = function
  | S i ->
      Array.blit ar 0 mem 0 16;
      for j = 0 to 15 do
        ar.((j + i) mod 16) <- mem.(j)
      done
  | P (x, y) -> Array.swap ar (Array.index_of ar x) (Array.index_of ar y)
  | X (x, y) -> Array.swap ar x y

let main n l =
  let ar = Array.init 16 (fun i -> char_of_int (int_of_char 'a' + i)) in
  let mem = Array.make 16 'a' in
  let l = parse l in
  let seen = Hashtbl.create 1024 in
  let seen_pos = Hashtbl.create 1024 in
  let rec repeat = function
    | 0 -> String.of_array ar
    | n ->
        Hashtbl.add seen (String.of_array ar) n;
        Hashtbl.add seen_pos n (String.of_array ar);
        List.iter (move mem ar) l;
        if Hashtbl.mem seen (String.of_array ar) then
          let i = Hashtbl.find seen (String.of_array ar) in
          Hashtbl.find seen_pos (i + 1 - (n mod (i + 1 - n)))
        else repeat (n - 1)
  in
  repeat n

let p1 = main 1
let p2 = main 1_000_000_000
