let run d f =
  let f_name = "../txt/day" ^ Printf.sprintf "%02d" d ^ ".txt" in
  f (Read_file.read_lines_string f_name)

let run_print y d part p =
  let t, sol = Utils.time_it (fun _ -> run d p) in
  Printf.printf "(%.2fs) Solution of y%d/%02d part %d is %s\n" t y d part sol;
  sol

let test_aux y d part p =
  let t, sol = Utils.time_it (fun _ -> run d p) in
  Printf.printf "(%.2fs) Solution of y%d/%02d part %d is %s\n" t y d part sol

let test_day y d p1 p2 =
  test_aux y d 1 p1;
  test_aux y d 2 p2

module type M = sig
  val p1 : string list -> string
  val p2 : string list -> string
end

exception TestFailure

let rec cat_str = function
  | [] -> ""
  | [ x ] -> x
  | x :: xs -> x ^ "\n" ^ cat_str xs

let my_assert y d part (e2 : string) =
  let fname n = "../expected/day" ^ Printf.sprintf "%02d.p%d.txt" d n in
  let read_expected n = Read_file.read_lines_string (fname n) |> cat_str in
  let e1 = read_expected part in
  if e1 <> e2 then (
    let msg =
      Printf.eprintf "y%02d/%02d part %d\nExpected \"%s\"\nFound    \"%s\"\n"
    in
    msg y d part e1 e2;
    Printf.eprintf "Please promote y20%02d/expected/day%02d.p%d.txt\n" y d part;
    raise TestFailure)

let test_expected y d m =
  let call p (module X : M) = if p = 1 then X.p1 else X.p2 in
  let run i p = run_print y i p (call p m) in
  my_assert y d 1 (run d 1);
  my_assert y d 2 (run d 2)
