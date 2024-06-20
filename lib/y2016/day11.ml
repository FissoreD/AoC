let parse_line l =
  let extract c i = String.split_on_char c i |> List.hd in
  let extract rex sep = Utils.find_all rex l |> List.map (extract sep) in
  let chips = extract {|[a-z]+-compatible|} '-' in
  let gen = extract {|[a-z]+ generator|} ' ' in
  chips @ gen |> List.length

let count_mvt conf =
  let total_layer = List.length conf - 1 in
  let sum = List.sum conf in
  let inter_mvn = List.mapi (fun i n -> i * n) conf in
  let inter_mvn = List.sum inter_mvn in
  (((2 * sum) - 3) * total_layer) - (2 * inter_mvn)

let p1 l = List.map parse_line l |> count_mvt |> string_of_int

let p2 l =
  let l = List.map parse_line l in
  (List.hd l + 4) :: List.tl l |> count_mvt |> Format.asprintf "%d"
