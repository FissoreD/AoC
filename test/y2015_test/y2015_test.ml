let int_to_string n =
  Printf.sprintf "%02d" n

let test_day day_nb =
  let open Y2015 in
  let f_name = "day" ^ int_to_string day_nb ^ ".txt" in
  let p1_list = [Day01.p1; Day02.p1] in
  let p2_list = [Day01.p2; Day02.p2] in
  let p1 = List.nth p1_list (day_nb - 1) in
  let p2 = List.nth p2_list (day_nb - 1) in
  Printf.printf "Solution of day %d part 1 is " day_nb;   
  p1 (Read_file.Main.read_lines_string f_name);
  print_newline ();
  Printf.printf "Solution of day %d part 2 is " day_nb;   
  p2 (Read_file.Main.read_lines_string f_name);
  print_newline ()

let _ =
  test_day 1;
  test_day 2