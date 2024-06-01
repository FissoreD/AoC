let pp_list ?(endL="") ?(sep=" ") ?(delimit=false) f l =
  let open Format in
  let hd = if delimit then "[" else "" in
  let tl = if delimit then "]" else "" in
  asprintf "%s%a%s%s" hd (pp_print_list ~pp_sep:(fun fmt _ -> pp_print_string fmt sep) 
    (fun fmt x -> Format.fprintf fmt "%s" (f x))) l tl endL


let pp_array ?(endL="") ?(sep=" ") ?(delimit=false) f l =
  pp_list ~endL ~sep ~delimit f (Array.to_list l)
