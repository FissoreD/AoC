type mode = Rect | Col | Row

let parse_line l =
  let ints = Utils.all_ints l in
  let i1, i2 = List.(hd ints, hd (tl ints)) in
  if l.[1] = 'e' then (Rect, i1, i2)
  else if l.[7] = 'c' then (Col, i1, i2)
  else (Row, i1, i2)

let update g1 =
  let height = Array.length g1 in
  let width = Array.length g1.(0) in
  function
  | Rect, w, h ->
      for i = 0 to w - 1 do
        for j = 0 to h - 1 do
          g1.(j).(i) <- 1
        done
      done
  | Row, r, shift ->
      let mem = Array.init width (fun i -> g1.(r).(i)) in
      for i = 0 to width - 1 do
        g1.(r).(i) <- mem.((width + i - shift) mod width)
      done
  | Col, c, shift ->
      let mem = Array.init height (fun i -> g1.(i).(c)) in
      for i = 0 to height - 1 do
        g1.(i).(c) <- mem.((height + i - shift) mod height)
      done

let make_screen l =
  let l = List.map parse_line l in
  let make_ar () = Array.init 6 (fun _ -> Array.make 50 0) in
  let grid = make_ar () in
  List.iter (update grid) l;
  grid

let p1 l =
  let grid = make_screen l in
  Array.fold_left (Array.fold_left ( + )) 0 grid |> string_of_int

let p2 l =
  let grid = make_screen l in
  let i2s i = if i = 1 then "#" else " " in
  let t = Printers.(pp_array (pp_array ~sep:"" ~endL:"\n" i2s)) grid in
  "\n " ^ t
