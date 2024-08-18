let instr =
  let set mem c v = mem.(c) <- v in
  let bin is_imm op (a, b, c) mem () =
    set mem c (op mem.(a) (if is_imm then b else mem.(b)))
  in
  let bin_op = [ ( + ); ( * ); ( land ); ( lor ) ] in
  let st is_imm (a, _, c) mem () = set mem c (if is_imm then a else mem.(a)) in
  let stl = [ st true; st false ] in
  let opir op (a, b, c) mem () = set mem c (if op a mem.(b) then 1 else 0) in
  let opri op (a, b, c) mem () = set mem c (if op mem.(a) b then 1 else 0) in
  let oprr op (a, b, c) mem () =
    set mem c (if op mem.(a) mem.(b) then 1 else 0)
  in
  let gt = [ opir ( > ); opri ( > ); oprr ( > ) ] in
  let eq = [ opir ( = ); opri ( = ); oprr ( = ) ] in
  gt @ eq @ List.map (bin true) bin_op @ List.map (bin false) bin_op @ stl

let parse l =
  let ints2t4 s =
    let k, a, b, c = Utils.all_ints s |> Tuple.to4 in
    (k, (a, b, c))
  in
  let rec parse_bef_aft acc = function
    | "" :: l -> parse_bef_aft acc l
    | x :: y :: z :: _ :: l when x.[0] = 'B' ->
        let bef, act, aft = (Utils.all_ints x, ints2t4 y, Utils.all_ints z) in
        parse_bef_aft ((bef, act, aft) :: acc) l
    | l -> (acc, l)
  in
  let bef_aft, tl = parse_bef_aft [] l in
  (bef_aft, List.map ints2t4 tl)

let rec op_assoc l =
  if l = [] then []
  else
    let ok, todo = List.partition (fun (_, e) -> List.length !e = 1) l in
    assert (ok <> []);
    let ok = List.map (fun (a, b) -> (a, List.hd !b)) ok in
    List.iter
      (fun (_, f) ->
        List.iter (fun (_, l) -> l := List.filter (( != ) f) !l) todo)
      ok;
    ok @ op_assoc todo

let is_valid_bef_aft (bef, (_, act), aft) instr =
  let ar = Array.of_list bef in
  instr act ar ();
  Array.to_list ar = aft

let rec find_instr l = function
  | [] -> op_assoc l
  | ((_, (i, _), _) as ii) :: tl ->
      let assoc = List.assoc i l in
      assoc := List.filter (is_valid_bef_aft ii) !assoc;
      find_instr l tl

let p1 l =
  let bef_aft, _ = parse l in
  List.map (fun ba -> List.filter (is_valid_bef_aft ba) instr) bef_aft
  |> List.filter (fun e -> List.length e > 2)
  |> List.length |> string_of_int

let rec run prog p =
  if p >= 0 && p < Array.length prog then (
    prog.(p) ();
    run prog (p + 1))

let p2 l =
  let all_instrs = List.mapi (fun a _ -> (a, ref instr)) instr in
  let bef_aft, prog = parse l in
  let op_instr_assoc = find_instr all_instrs bef_aft in
  let m = Array.make 4 0 in
  let prog = List.(map (fun (k, act) -> assoc k op_instr_assoc act m)) prog in
  run (Array.of_list prog) 0;
  string_of_int m.(0)
