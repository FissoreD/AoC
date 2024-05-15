open Y2015

let p1_list = [Day01.p1; Day02.p1; Day03.p1; Day04.p1; Day05.p1]
let p2_list = [Day01.p2; Day02.p2; Day03.p2; Day04.p2; Day05.p2] 

let int_to_string n =
  Printf.sprintf "%02d" n

let test_day day_nb =
  let f_name = "day" ^ int_to_string day_nb ^ ".txt" in
  let p1 = List.nth p1_list (day_nb - 1) in
  let p2 = List.nth p2_list (day_nb - 1) in
  Printf.printf "Solution of day %d part 1 is " day_nb;   
  p1 (Read_file.Main.read_lines_string f_name);
  print_newline ();
  Printf.printf "Solution of day %d part 2 is " day_nb;   
  p2 (Read_file.Main.read_lines_string f_name);
  print_newline ()

let _ =
  let day_nb = 5 in
  for i = 1 to day_nb do
    test_day i
  done