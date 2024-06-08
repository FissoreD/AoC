type pos = {x:int; y:int}

let move p = function
  | '^' -> {p with y = p.y - 1}
  | 'v' -> {p with y = p.y + 1}
  | '>' -> {p with x = p.x + 1}
  | '<' -> {p with x = p.x - 1}
  | _ -> invalid_arg "invalid move"

let add_pos p l = 
  if not (List.mem !p !l) then
      l := !p :: !l

let p1 s = 
  let s = List.hd s in
  let c_pos = ref {x=0;y=0} in
  let explored = ref [!c_pos] in
  let len = String.length s in
  for i = 0 to len - 1 do
    c_pos := move !c_pos s.[i];
    add_pos c_pos explored
  done;
  string_of_int @@ List.length !explored

let p2 s = 
  let s = List.hd s in
  let c_pos = ref {x=0;y=0} in
  let c_pos' = ref {x=0;y=0} in
  let explored = ref [!c_pos] in
  let len = String.length s in
  for i = 0 to len - 1 do
    if i mod 2 == 0 then (c_pos := move !c_pos s.[i]; add_pos c_pos explored)
    else (c_pos' := move !c_pos' s.[i]; add_pos c_pos' explored)
  done;
  string_of_int @@ List.length !explored


