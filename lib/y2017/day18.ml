let c2i c = int_of_char c.[0] - int_of_char 'a'

type t = Snd of int | Rcv of int | Jnz of int | Unit [@@deriving show]

let parse l =
  let get ar n = if Utils.is_int n then int_of_string n else ar.(c2i n) in
  let binary ar i op v = snd ((ar.(i) <- op ar.(i) v), Unit) in
  match String.split_on_char ' ' l with
  | [ "set"; c; n ] -> fun ar -> snd ((ar.(c2i c) <- get ar n), Unit)
  | [ "mul"; c; n ] -> fun ar -> binary ar (c2i c) ( * ) (get ar n)
  | [ "add"; c; n ] -> fun ar -> binary ar (c2i c) ( + ) (get ar n)
  | [ "mod"; c; n ] -> fun ar -> binary ar (c2i c) ( mod ) (get ar n)
  | [ "snd"; n ] -> fun ar -> Snd (get ar n)
  | [ "rcv"; c ] -> fun _ -> Rcv (c2i c)
  | [ "jgz"; n; m ] -> fun ar -> if get ar n > 0 then Jnz (get ar m) else Unit
  | _ -> failwith "Invalid input"

let rec exec_prog is_p1 l ar pos snd =
  if pos >= Array.length l || pos < 0 then ((pos, -1), snd)
  else
    match l.(pos) ar with
    | Rcv n when is_p1 && ar.(n) = 0 -> exec_prog is_p1 l ar (pos + 1) snd
    | Rcv x -> ((pos, x), snd)
    | Jnz n -> exec_prog is_p1 l ar (pos + n) snd
    | Snd n when is_p1 -> exec_prog is_p1 l ar (pos + 1) n
    | Snd n -> ((pos + 1, -1), n)
    | Unit -> exec_prog is_p1 l ar (pos + 1) snd

let p1 l =
  let l = Array.map parse (Array.of_list l) in
  exec_prog true l (Array.make 26 0) 0 0 |> snd |> string_of_int

let p2 l =
  let l = Array.map parse (Array.of_list l) in
  let p0, p1 = (Array.make 26 0, Array.make 26 0) in
  let qp0, qp1 = (Queue.create (), Queue.create ()) in
  p1.(c2i "p") <- 1;

  (* Run the program until block *)
  let rec run cont (p, pos', qr, qw) n =
    let (pos, id), v = exec_prog false l p pos' 0 in
    if id = -1 then (
      Queue.push v qw;
      run true (p, pos, qr, qw) (n + 1))
    else if Queue.is_empty qr then (cont || pos <> pos', pos, n)
    else (
      p.(id) <- Queue.pop qr;
      run true (p, pos + 1, qr, qw) n)
  in

  (* Run p0 and p1 until block *)
  let rec aux pn pos0 pos1 n =
    let arg = if pn = 0 then (p0, pos0, qp0, qp1) else (p1, pos1, qp1, qp0) in
    let cont, pos, n' = run false arg n in
    if cont then if pn = 0 then aux 1 pos pos1 n else aux 0 pos0 pos n'
    else if pn = 0 then n
    else n'
  in

  aux 0 0 0 0 |> string_of_int
