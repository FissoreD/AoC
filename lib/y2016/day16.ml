let ascii0 = int_of_char '0'

let parse ~max_len l =
  let l = List.hd l in
  let len = String.length l in
  let c2i c = int_of_char c - ascii0 in
  let f i = if i < len then c2i l.[i] else 0 in
  (Array.init max_len f, len)

let update ~max_len ar pos =
  ar.(pos) <- 0;
  let new_len = (pos * 2) + 1 in
  let stop p = p = new_len || p = max_len in
  let rec aux bef aft =
    if not (stop aft) then (
      ar.(aft) <- 1 - ar.(bef);
      aux (bef - 1) (aft + 1))
  in
  aux (pos - 1) (pos + 1);
  new_len

let rec fill_ar ~max_len (ar, len) =
  if len < max_len then fill_ar ~max_len (ar, update ~max_len ar len) else ar

let rec checksum ar stop pos i =
  if i + 1 < stop then (
    ar.(pos) <- (if ar.(i) = ar.(i + 1) then 1 else 0);
    checksum ar stop (pos + 1) (i + 2))

let rec checksumi (ar, len) =
  if len mod 2 = 1 then String.init len (fun i -> char_of_int (ar.(i) + ascii0))
  else (
    checksum ar len 0 0;
    checksumi (ar, len / 2))

let main max_len l = (fill_ar ~max_len (parse ~max_len l), max_len) |> checksumi
let p1 = main 272
let p2 = main 35651584
