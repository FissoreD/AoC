let parse = function
  | [] | [ _ ] -> Utils.error 2017 25 "0"
  | start :: checksum :: tl ->
      let dir s = if String.ends_with ~suffix:"right." s then 1 else -1 in
      let getc s = s.[String.length s - 2] in
      let start = getc start in
      let checksum = List.hd (Utils.all_ints checksum) in
      let parse_block = function
        | _ :: write :: mv :: cont :: tl -> ((getc write, dir mv, getc cont), tl)
        | _ -> Utils.error 2017 25 "1"
      in
      let rec parse_state = function
        | [] -> []
        | _empty :: state :: l ->
            let if0, l = parse_block l in
            let if1, l = parse_block l in
            (getc state, (if0, if1)) :: parse_state l
        | _ -> Utils.error 2017 25 "2"
      in
      (start, checksum, parse_state tl)

let get tbl k = Hashtbl.find_opt tbl k |> Option.value ~default:'0'
let fst (a, _, _) = a
let snd (_, a, _) = a
let thd (_, _, a) = a
let add_char acc = function '0' -> acc | _ -> acc + 1

let rec run tm iter_nb cs tbl pos state =
  if iter_nb >= cs then Hashtbl.to_seq_values tbl |> Seq.fold_left add_char 0
  else
    let if0, if1 = List.assoc state tm in
    let op = if get tbl pos = '0' then if0 else if1 in
    Hashtbl.replace tbl pos (fst op);
    run tm (iter_nb + 1) cs tbl (pos + snd op) (thd op)

let p1 l =
  let state, checksum, tm = parse l in
  run tm 0 checksum (Hashtbl.create 1024) 0 state |> string_of_int

let p2 _ = ""
