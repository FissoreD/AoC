include String

let of_array ar = String.init (Array.length ar) (fun i -> ar.(i))
let to_array s = Array.init (String.length s) (fun i -> s.[i])
let to_list s = List.init (String.length s) (fun i -> s.[i])
