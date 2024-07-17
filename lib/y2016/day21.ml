let swap_pos x y ar =
  let a, b = (ar.(x), ar.(y)) in
  ar.(x) <- b;
  ar.(y) <- a

let swap_letter scramble x y ar =
  let p1, p2 = (Array.index_of ar x, Array.index_of ar y) in
  if scramble then swap_pos p1 p2 ar else swap_pos p2 p1 ar

let rotate_ar scramble n ar =
  let len = Array.length ar in
  let n = if scramble then n else ~-n in
  let n = if n >= 0 then n else len - (abs n mod len) in
  let copy = Array.init len (fun i -> ar.(i)) in
  for i = 0 to len - 1 do
    ar.((i + n) mod len) <- copy.(i mod len)
  done

let rotate_lett scramble c ar =
  let id = Array.index_of ar c in
  let get_pos p = p + 1 + if p >= 4 then 1 else 0 in
  if scramble then rotate_ar true (get_pos id) ar
  else
    (* 0 -> 1; 1 -> 3; 2 -> 5; 3 -> 7; 4 -> 2; 5 -> 4; 6 -> 6; 7 -> 0  *)
    let even2rot n = if n = 0 then 1 else 5 + (n / 2) in
    let rot = if id mod 2 = 1 then (id + 1) / 2 else even2rot id in
    rotate_ar false rot ar

let reverse i1 i2 ar =
  let mid = (i2 - i1) / 2 in
  for i = 0 to mid do
    swap_pos (i1 + i) (i2 - i) ar
  done

let move scramble i1 i2 ar =
  let i1, i2 = if scramble then (i1, i2) else (i2, i1) in
  for i = i1 + 1 to i2 do
    swap_pos (i - 1) i ar
  done;
  for i = i1 - 1 downto i2 do
    swap_pos (i + 1) i ar
  done

let parse_line is_p1 l : char array -> unit =
  let s2i = int_of_string in
  match String.split_on_char ' ' l with
  | [ "swap"; "position"; x; _; _; y ] -> swap_pos (s2i x) (s2i y)
  | [ "swap"; "letter"; x; _; _; y ] -> swap_letter is_p1 x.[0] y.[0]
  | [ "rotate"; "right"; x; _ ] -> rotate_ar is_p1 (s2i x)
  | [ "rotate"; "left"; x; _ ] -> rotate_ar is_p1 (-s2i x)
  | [ "rotate"; _; _; _; _; _; x ] -> rotate_lett is_p1 x.[0]
  | [ "reverse"; _; x; _; y ] -> reverse (s2i x) (s2i y)
  | [ "move"; _; x; _; _; y ] -> move is_p1 (s2i x) (s2i y)
  | _ -> Utils.error 2016 21 l

let main s l is_p1 =
  let ar = String.to_array s in
  let instr = List.map (parse_line is_p1) l in
  List.iter (fun x -> x ar) instr;
  String.of_array ar

let p1 l = main "abcdefgh" l true
let p2 l = main "fbgdceah" (List.rev l) false
