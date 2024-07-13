include String

let of_array ar = String.init (Array.length ar) (fun i -> ar.(i))

let of_list =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (Printf.sprintf "%s%c" acc hd) tl
  in
  aux ""

let to_array s = Array.init (String.length s) (fun i -> s.[i])
let to_list s = List.init (String.length s) (fun i -> s.[i])
